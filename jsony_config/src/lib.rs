//! Jsony Config is an opinionated application/service configuration framework based on [jsony].
//!
//! **Features:**
//!
//! - Fast compile times, a thin layer over [jsony].
//! - Lenient Json Configs Supporting ([jsony::JsonParserConfig])
//!   - Trailing Commas
//!   - Comments
//!   - Unquoted static keys
//! - Loading from multiple config files with priority.
//! - Resolving paths relative to the location of the config file ([relative_path])
//! - Warnings for duplicate and unused fields, localized to the specific file and line ([Diagnostic])
//! - Various search strategies for finding configs ([Search])
//! - Lazy initialized global config, useable from tests ([GlobalConfig])
//!
//! ## Configuration Formats
//!
//! Two configuration file formats are supported: `.json` and `.js`.
//!
//! ### JSON (`.json`)
//!
//! The `.json` format supports lenient features like trailing commas and comments, which can be useful for configuration files. However, editor support for these extensions to the JSON standard can be inconsistent and may require special configuration.
//!
//! ```json
//! // application.config.json
//! {
//!   "number": 32,
//!   // A useful comment
//!   "test_output": "./output"
//! }
//! ```
//!
//! ### JavaScript (`.js`)
//!
//! To provide a better out-of-the-box editor experience with syntax highlighting and validation for lenient JSON features, a `.js` format is also supported.
//! This format is a workaround that uses a subset of JavaScript syntax.
//!
//! The configuration must be assigned to a `const CONFIG =` declaration. `jsony_config` will locate this line and parse the object that follows.
//!
//! ```javascript
//! // application.config.js
//! const CONFIG = {
//!   number: 32,
//!   // A useful comment
//!   test_output: "./output",
//! };
//! ```
//!
//! **Note:** While this looks like JavaScript, it is not executed as such. JavaScript features like variables, functions, or arithmetic are not supported.
//! This approach is simply a "hack" to leverage editor support for JavaScript object literals, which closely resembles the lenient JSON syntax.
//!
//! ### Example
//!
//! ```rust
//! use jsony_config::{Search, GlobalConfig, relative_path};
//!
//! #[derive(jsony::Jsony, Debug)]
//! #[jsony(Flattenable)]
//! pub struct Config {
//!    #[jsony(default = 42)]
//!    number: u32,
//!    #[jsony(with = relative_path)]
//!    test_output: Option<std::path::PathBuf>,
//! }
//!
//! static CONFIG: GlobalConfig<Config> = GlobalConfig::new(&[
//!     Search::Flag("--config"),
//!     Search::Upwards{
//!         file_stem: "application.config",
//!         override_file_stem: Some("application.local.config"),
//!     },
//! ]);
//!
//! fn main() {
//!     CONFIG.initialize(&mut jsony_config::print_diagnostics).unwrap();
//!     println!("{:#?}", CONFIG);
//!     assert_eq!(CONFIG.number, 42)
//! }
//! ```

use std::{
    cell::RefCell,
    mem::MaybeUninit,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    ptr::NonNull,
    sync::OnceLock,
};

use jsony::{
    JsonError, JsonParserConfig,
    json::{DecodeError, FieldVisitor, FromJsonFieldVisitor, Parser},
};

#[derive(Debug)]
/// Defines the strategy for locating configuration files.
pub enum Search<'a> {
    /// Looks for a configuration file path specified by a command-line flag.
    ///
    /// Example: `--config ../data/config.json`
    Flag(&'a str),
    /// Searches for a configuration file at a specific path relative to the current working directory.
    Path(&'a std::path::Path),
    /// Searches for a configuration file in the current working directory and then traverses up to parent directories.
    ///
    /// It looks for a file with `file_stem` (e.g., `application.config.json` or `.js`). If `override_file_stem` is provided,
    /// then if a config file with override stem is found next config file it will apply with it will high priority, useful
    /// for local configuration overrides.
    Upwards {
        file_stem: &'a str,
        override_file_stem: Option<&'a str>,
    },
}

/// Helper for parsing paths relative to the config file's location.
///
/// When a field is annotated with `#[jsony(with = relative_path)]`, its string value
/// will be interpreted as a path relative to the directory containing the config file
/// from which it was loaded. The path is then resolved to an absolute path.
///
/// # Example
///
/// ```
/// use std::path::PathBuf;
/// use jsony::Jsony;
/// use jsony_config::relative_path;
///
/// #[derive(Jsony)]
/// pub struct MyConfig {
///     #[jsony(with = relative_path)]
///     log_file: PathBuf,
/// }
/// ```
///
/// If the config file is at `/etc/myapp/config.json` and contains `"log_file": "logs/app.log"`,
/// the `log_file` field will be parsed as `/etc/myapp/logs/app.log`.
pub mod relative_path {
    use std::path::PathBuf;

    use jsony::{FromJson, TextWriter, ToJson, json::DecodeError};

    use crate::ConfigContext;

    pub fn encode_json<T: ToJson>(value: &T, output: &mut TextWriter) {
        value.encode_json__jsony(output);
    }

    /// When in the context of a `jsony_config` parse, this function decodes a string
    /// and resolves it as a path relative to the parent directory of the current config file.
    ///
    /// Outside of a `jsony_config` context, it decodes the path as-is.
    pub fn decode_json<T: From<PathBuf>>(
        parser: &mut jsony::parser::Parser<'_>,
    ) -> Result<T, &'static DecodeError> {
        pub fn decode_pathbuf(
            parser: &mut jsony::parser::Parser<'_>,
        ) -> Result<PathBuf, &'static DecodeError> {
            let mut path = PathBuf::decode_json(parser)?;
            if let Some(context) = unsafe { ConfigContext::current() } {
                path = context
                    .config_file
                    .parent()
                    .unwrap_or(&context.config_file)
                    .join(path);
                if let Ok(abs_path) = std::path::absolute(&path) {
                    path = abs_path;
                }
                if let Ok(canonical_path) = path.canonicalize() {
                    path = canonical_path;
                }
            }
            Ok(path)
        }
        Ok(decode_pathbuf(parser)?.into())
    }
}

/// A thread-safe, lazily initialized global configuration container.
///
/// It holds the application's configuration, which is loaded according to a `ConfigSearchStrategy`.
/// Accessing the configuration for the first time will trigger the search and parsing,
/// which will panic on failure.
///
/// It implements `Deref`, allowing for direct, transparent access to the inner config struct.
pub struct GlobalConfig<C: JsonyConfig> {
    config: std::sync::OnceLock<C>,
    strategy: &'static [Search<'static>],
    transform: Option<fn(&mut C) -> Result<(), Error>>,
}

impl<C: JsonyConfig + std::fmt::Debug> std::fmt::Debug for GlobalConfig<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(config) = self.config.get() {
            config.fmt(f)
        } else {
            f.write_str("GlobalConfig::None")
        }
    }
}

impl<C: JsonyConfig> GlobalConfig<C> {
    /// Eagerly initializes the global configuration.
    ///
    /// This method performs the configuration search and parsing immediately.
    /// If initialization fails, it will panic with a detailed error message.
    /// Diagnostics (warnings, errors) are passed to the provided `handler`.
    /// If the config is already initialized, this function does nothing.
    pub fn initialize(&self, handler: &mut DiagnosticHandler) -> Result<&C, Error> {
        if let Some(value) = self.config.get() {
            return Ok(value);
        }
        let result = load::<C>(&self.strategy, handler);
        match result {
            Ok(mut config) => {
                if let Some(transform) = self.transform {
                    if let Err(err) = transform(&mut config) {
                        handler(Diagnostic {
                            level: DiagnosticLevel::Error,
                            message: &format!("Failed to transform config: {:?}", err),
                            file: None,
                            line: None,
                        });
                        return Err(err);
                    }
                }
                Ok(self.config.get_or_init(|| config))
            }
            Err(err) => Err(err),
        }
    }
}

impl<C: JsonyConfig> GlobalConfig<C> {
    /// Creates a new, uninitialized `GlobalConfig`.
    ///
    /// The configuration will be loaded on first access or when `initialize` is called.
    /// The search options specified first are prioritized and the search will stop when the first match is found.
    pub const fn new(strategy: &'static [Search<'static>]) -> GlobalConfig<C> {
        GlobalConfig {
            config: OnceLock::new(),
            strategy,
            transform: None,
        }
    }

    /// Creates a new, uninitialized `GlobalConfig` with a transform function to be called on initialization.
    ///
    /// The configuration will be loaded on first access or when `initialize` is called.
    /// The search options specified first are prioritized and the search will stop when the first match is found.
    pub const fn new_with_transform(
        strategy: &'static [Search<'static>],
        transform: fn(&mut C) -> Result<(), Error>,
    ) -> GlobalConfig<C> {
        GlobalConfig {
            config: OnceLock::new(),
            strategy,
            transform: Some(transform),
        }
    }

    #[cold]
    #[inline(never)]
    fn lazy_init(&self) -> &C {
        #[cfg(feature = "kvlog")]
        let logger: &mut DiagnosticHandler = &mut kvlog_diagnostics;
        #[cfg(not(feature = "kvlog"))]
        let logger: &mut DiagnosticHandler = &mut print_diagnostics;
        match self.initialize(logger) {
            Ok(config) => config,
            Err(err) => panic!("Failed to lazy initialize config: {:?}", err),
        }
    }
}

impl<C: JsonyConfig> std::ops::Deref for GlobalConfig<C> {
    type Target = C;

    fn deref(&self) -> &Self::Target {
        // Note we avoid get_or_init here to encourage better codegen, with the #[cold], lazy_init
        if let Some(config) = self.config.get() {
            config
        } else {
            self.lazy_init()
        }
    }
}
/// A function type that handles diagnostics emitted during config parsing.
///
/// See [print_diagnostics] and [kvlog_diagnostics] for example implementations.
pub type DiagnosticHandler = dyn FnMut(Diagnostic);

#[derive(Debug)]
/// The severity level of a diagnostic message.
pub enum DiagnosticLevel {
    Info,
    Warn,
    Error,
}

#[cfg(feature = "kvlog")]
/// A `DiagnosticHandler` that emits structured logs using the `kvlog` crate.
///
/// This is only available when the `kvlog` feature is enabled.
pub fn kvlog_diagnostics(dia: Diagnostic) {
    use kvlog::encoding::Encode;
    let mut log = kvlog::global_logger();
    let mut fields = log.encoder.append_now(match dia.level {
        DiagnosticLevel::Info => kvlog::LogLevel::Info,
        DiagnosticLevel::Warn => kvlog::LogLevel::Warn,
        DiagnosticLevel::Error => kvlog::LogLevel::Error,
    });
    if let Some(line) = &dia.line {
        line.get().encode_log_value_into(fields.dynamic_key("line"));
    }
    if let Some(file) = &dia.file {
        (fields.dynamic_key("file")).value_via_display(&(file.display()));
    }
    // todo make get_static_key pub and const and then use it here
    dia.message.encode_log_value_into(fields.key("err"));
    module_path!().encode_log_value_into(fields.key("target"));
    "Parsing Config".encode_log_value_into(fields.key("msg"));
    fields.apply_current_span();
    log.poke();
}

/// A `DiagnosticHandler` that prints formatted messages to standard output.
///
/// This is the default handler when the `kvlog` feature is not enabled.
///
/// # Example Output
///
/// ```text
/// WARN: JSONY_CONFIG: Unused field `debug_mode` @ /path/to/config.json:12
/// ```
pub fn print_diagnostics(dia: Diagnostic) {
    let level = match dia.level {
        DiagnosticLevel::Info => "INFO",
        DiagnosticLevel::Warn => "WARN",
        DiagnosticLevel::Error => "ERROR",
    };
    print!("{}: JSONY_CONFIG: {}", level, dia.message);
    if let Some(file) = &dia.file {
        print!(" @ {}", file.display());
    }
    if let Some(line) = &dia.line {
        print!(":{line}");
    }
    println!();
}

#[derive(Debug)]
/// Diagnostic message (e.g., a warning or error) from the config loading process.
pub struct Diagnostic<'a> {
    /// The severity of the diagnostic.
    pub level: DiagnosticLevel,
    /// The diagnostic message. Example: `Unused Field: "data"`
    pub message: &'a str,
    /// The configuration file where the issue was found.
    pub file: Option<&'a Path>,
    /// The line number in the file where the issue occurred.
    pub line: Option<NonZeroUsize>,
}
/// A trait that must be implemented by the root configuration struct.
///
/// This trait is implemented automatically for any type that derives `jsony::Jsony`.
/// It is recommended to derive it via `#[jsony(Flattenable)]` on your main config struct.
///
/// # Example
///
/// ```
/// use jsony::Jsony;
/// use jsony_config::JsonyConfig;
///
/// #[derive(Jsony, Debug)]
/// #[jsony(Flattenable)] // This enables the implementation of JsonyConfig
/// pub struct MyConfig {
///     // ... fields
/// }
/// ```
#[diagnostic::on_unimplemented(note = "You can derive JsonyConfig via `#[jsony(Flattenable)]`")]
pub trait JsonyConfig {
    #[doc(hidden)]
    unsafe fn config_field_visitor<'a>(
        config: NonNull<()>,
    ) -> jsony::__internal::DynamicFieldDecoder<'a>;
}

#[diagnostic::do_not_recommend]
impl<T: for<'a> FromJsonFieldVisitor<'a, Visitor = jsony::__internal::DynamicFieldDecoder<'a>>>
    JsonyConfig for T
{
    unsafe fn config_field_visitor<'a>(
        config: NonNull<()>,
    ) -> jsony::__internal::DynamicFieldDecoder<'a> {
        let parser = Parser::new("", JsonParserConfig::default());
        let visitor = unsafe { T::new_field_visitor(config, &parser) };
        visitor
    }
}

fn load_config_file(output: &mut Vec<ConfigFile>, path: PathBuf) -> Result<(), Error> {
    let raw_contents = match std::fs::read_to_string(&path) {
        Ok(value) => value,
        Err(err) => {
            return Err(Error::IOError(path.clone(), err));
        }
    };
    let prelude_length = if path.extension().is_some_and(|ext| ext == "js") {
        let header = "const CONFIG =";
        if let Some(idx) = raw_contents.find(header) {
            idx + header.len()
        } else {
            // todo make this an error
            0
        }
    } else {
        0
    };
    output.push(ConfigFile {
        path,
        raw_contents,
        prelude_length,
    });
    Ok(())
}

impl<'a> Search<'a> {
    fn search_until_found(
        many: &'a [Search<'a>],
        output: &mut Vec<ConfigFile>,
        cwd: &std::path::Path,
    ) -> Result<(), Error> {
        for strategy in many {
            strategy.search(output, cwd)?;
            if !output.is_empty() {
                return Ok(());
            }
        }
        Ok(())
    }
    fn search(&self, output: &mut Vec<ConfigFile>, cwd: &std::path::Path) -> Result<(), Error> {
        match self {
            Search::Flag(flag) => {
                let mut args = std::env::args();
                let _ = args.by_ref().find(|a| a == flag);
                // todo maybe allow specifying multiple times.
                if let Some(config) = args.next() {
                    return load_config_file(output, cwd.join(config));
                }
            }
            Search::Path(path) => {
                if path.exists() {
                    return load_config_file(output, cwd.join(path));
                }
                return Ok(());
            }
            Search::Upwards {
                file_stem,
                override_file_stem,
            } => {
                let mut cwd = cwd.to_path_buf();
                let filename = format!("{file_stem}.js");
                loop {
                    let mut path = cwd.join(&filename);
                    for _ in 0..2 {
                        if !path.exists() {
                            path.set_extension("json");
                            continue;
                        }
                        if let Some(override_file_stem) = override_file_stem {
                            let mut override_path = cwd.join(format!("{override_file_stem}.js"));
                            if override_path.exists() {
                                load_config_file(output, override_path)?;
                            } else {
                                override_path.set_extension("json");
                                if override_path.exists() {
                                    load_config_file(output, override_path)?;
                                }
                            }
                        }
                        return load_config_file(output, path);
                    }
                    if !cwd.pop() {
                        break;
                    }
                }
            }
        }
        Ok(())
    }
}

struct ConfigContext {
    config_file: PathBuf,
    handler: RefCell<&'static mut DiagnosticHandler>,
    config_line_offset: usize,
}

thread_local! {
    static CONFIG_CONTEXT: std::cell::Cell<Option<&'static ConfigContext>> = const {std::cell::Cell::new(None)};
}

struct ConfigContextDropGuard;

impl Drop for ConfigContextDropGuard {
    fn drop(&mut self) {
        CONFIG_CONTEXT.set(None)
    }
}

impl ConfigContext {
    /// Safety: The lifetime is fictitious and the returned reference must not leave
    /// the function containing the current call.
    unsafe fn current() -> Option<&'static ConfigContext> {
        CONFIG_CONTEXT.get()
    }
}

struct ConfigFile {
    path: PathBuf,
    raw_contents: String,
    prelude_length: usize,
}

impl ConfigFile {
    fn prelude(&self) -> &str {
        &self.raw_contents[..self.prelude_length]
    }
    fn config_text(&self) -> &str {
        &self.raw_contents[self.prelude_length..]
    }
}

/// Loads a configuration of type `T` from the specified search locations.
///
/// The config will be loaded from the first `Search` strategy that finds a match.
/// See [GlobalConfig] for a wrapper type for loading into a static global variable.
pub fn load<T: JsonyConfig>(
    locations: &[Search],
    diagnostic_handler: &mut DiagnosticHandler,
) -> Result<T, Error> {
    let mut conf = MaybeUninit::<T>::uninit();
    let mut configs = Vec::new();
    let cwd = match std::env::current_dir() {
        Ok(cwd) => cwd,
        Err(err) => return Err(Error::Other(err.to_string())),
    };
    if let Err(err) = Search::search_until_found(locations, &mut configs, &cwd) {
        return Err(err);
    };
    if configs.is_empty() {
        diagnostic_handler(Diagnostic {
            level: DiagnosticLevel::Warn,
            message: &format!(
                "Using Default config, no configuration files found from: {:#?}",
                locations
            ),
            file: None,
            line: None,
        })
    }
    let mut visitor =
        unsafe { T::config_field_visitor(NonNull::new_unchecked(conf.as_mut_ptr().cast())) };
    match initialize_config_internal(&configs, &mut visitor, diagnostic_handler) {
        Ok(()) => unsafe { Ok(conf.assume_init()) },
        Err(err) => Err(err),
    }
}

fn initialize_config_internal<'a>(
    paths: &'a [ConfigFile],
    visitor: &mut jsony::__internal::DynamicFieldDecoder<'a>,
    handler: &mut DiagnosticHandler,
) -> Result<(), Error> {
    let res = unsafe { initialize_configs_inner(paths, visitor, handler) };
    match res {
        Ok(()) => match visitor.complete() {
            Ok(()) => Ok(()),
            Err(err) => {
                if err == &jsony::error::MISSING_REQUIRED_FIELDS {
                    let missing = !visitor.bitset & visitor.required;
                    let mut message = format!("Missing required root config fields: [");
                    for (i, field) in visitor.schema.fields().iter().enumerate() {
                        if missing & (1 << i) != 0 {
                            use std::fmt::Write;
                            let _ = write!(message, "\n    {:?},", field.name);
                        }
                    }
                    message.push_str("\n]");
                    handler(Diagnostic {
                        level: DiagnosticLevel::Error,
                        message: &message,
                        file: None,
                        line: None,
                    });
                    return Err(Error::Other(message));
                }
                return Err(Error::JsonError(JsonError::new(err, None)));
            }
        },
        Err(err) => unsafe {
            visitor.destroy();
            Err(err)
        },
    }
}

/// This type represents all possible errors thatcan occur when loading configurations.
pub enum Error {
    JsonError(jsony::JsonError),
    IOError(PathBuf, std::io::Error),
    Other(String),
    Custom(Box<dyn std::error::Error + Send + Sync>),
}

impl<T: Into<Box<dyn std::error::Error + Send + Sync>>> From<T> for Error {
    fn from(value: T) -> Self {
        Error::Custom(value.into())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Error as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::JsonError(arg0) => f.debug_tuple("JsonError").field(arg0).finish(),
            Self::IOError(arg0, arg1) => f.debug_tuple("IOError").field(arg0).field(arg1).finish(),
            Self::Other(arg0) => f.write_str(arg0),
            Self::Custom(arg0) => arg0.fmt(f),
        }
    }
}

unsafe fn initialize_configs_inner<'a>(
    configs: &'a [ConfigFile],
    decoder: &mut jsony::__internal::DynamicFieldDecoder<'a>,
    handler: &mut DiagnosticHandler,
) -> Result<(), Error> {
    let mut ctx = ConfigContext {
        config_file: PathBuf::default(),
        config_line_offset: 0,
        // Safety: We only lie about the lifetime to store it a thread local which will be unset
        // by the `ConfigContextDropGuard Below`
        handler: RefCell::new(unsafe {
            std::mem::transmute::<&mut DiagnosticHandler, &mut DiagnosticHandler>(handler)
        }),
    };
    'to_next_config_file: for config in configs {
        {
            ctx.config_file.clone_from(&config.path);
            ctx.config_line_offset = config.prelude().matches('\n').count();
        }
        let ctx: &ConfigContext = &ctx;
        let mut duplicate_ignore = decoder.bitset;

        let _guard = ConfigContextDropGuard;
        // Safety: The _guard above will unset CONFIG_CONTEXT at the end
        // the scope. Accessing CONFIG_CONTEXT is guarded by unsafe.
        unsafe {
            CONFIG_CONTEXT.set(Some(&*(ctx as *const ConfigContext)));
        }

        let mut parser = jsony::parser::Parser::new(
            &config.config_text(),
            JsonParserConfig {
                allow_comments: true,
                allow_unquoted_field_keys: true,
                allow_trailing_data: true,
                allow_trailing_commas: true,
                ..Default::default()
            },
        );

        parser.attach_unused_field_hook(|info| {
            if let Some(ctx) = unsafe { ConfigContext::current() } {
                let message = format!("Unused field: `{}`", info.key());
                let parser = info.into_parser();
                let prefix = &parser.at.ctx.as_bytes()[..parser.at.index];
                let lines =
                    prefix.iter().filter(|ch| **ch == b'\n').count() + ctx.config_line_offset;
                ctx.handler.borrow_mut()(Diagnostic {
                    level: DiagnosticLevel::Warn,
                    file: Some(&ctx.config_file),
                    line: Some(NonZeroUsize::MIN.saturating_add(lines)),
                    message: &message,
                });
            }
        });
        let error: &DecodeError = 'err: {
            match parser.at.enter_object(&mut parser.scratch) {
                Ok(Some(mut key)) => 'key_loop: loop {
                    'next: {
                        'unused: {
                            let (index, field) = 'found: {
                                let fields = decoder.schema.fields();
                                for (index, field) in fields.iter().enumerate() {
                                    if field.name != key {
                                        continue;
                                    }
                                    break 'found (index, field);
                                }
                                for (index, alias_name) in decoder.alias {
                                    if *alias_name == key {
                                        break 'found (*index, &fields[*index]);
                                    }
                                }
                                {
                                    if let Some(ctx) = unsafe { ConfigContext::current() } {
                                        let message = format!("Unused field: `{}`", key);
                                        let prefix = &parser.at.ctx.as_bytes()[..parser.at.index];
                                        let lines =
                                            prefix.iter().filter(|ch| **ch == b'\n').count()
                                                + ctx.config_line_offset;
                                        ctx.handler.borrow_mut()(Diagnostic {
                                            level: DiagnosticLevel::Warn,
                                            file: Some(&ctx.config_file),
                                            line: Some(NonZeroUsize::MIN.saturating_add(lines)),
                                            message: &message,
                                        });
                                    }
                                }
                                break 'unused;
                            };
                            let mask = 1 << index;
                            if decoder.bitset & mask != 0 {
                                if mask & duplicate_ignore != 0 {
                                    duplicate_ignore ^= mask;
                                } else {
                                    let message = format!(
                                        "Duplicate field in same file is ignored: `{}`",
                                        key
                                    );
                                    let prefix = &parser.at.ctx.as_bytes()[..parser.at.index];
                                    let lines = prefix.iter().filter(|ch| **ch == b'\n').count()
                                        + ctx.config_line_offset;
                                    ctx.handler.borrow_mut()(Diagnostic {
                                        level: DiagnosticLevel::Warn,
                                        file: Some(&ctx.config_file),
                                        line: Some(NonZeroUsize::MIN.saturating_add(lines)),
                                        message: &message,
                                    });
                                }
                                break 'unused;
                            }
                            if let Err(err) = unsafe {
                                (field.decode)(
                                    decoder.destination.byte_add(field.offset),
                                    &mut parser,
                                )
                            } {
                                break 'err err;
                            }
                            decoder.bitset |= mask;
                            break 'next;
                        }

                        if let Err(error) = parser.at.skip_value() {
                            return Err(Error::JsonError(JsonError::extract(error, &mut parser)));
                        }
                    }

                    match parser.at.object_step(&mut parser.scratch) {
                        Ok(Some(next_key2)) => {
                            key = next_key2;
                            continue 'key_loop;
                        }
                        Ok(None) => break 'key_loop,
                        Err(err) => break 'err err,
                    }
                },
                Ok(None) => {}
                Err(err) => break 'err err,
            };
            continue 'to_next_config_file;
        };
        let err = JsonError::extract(error, &mut parser);
        let beat = parser.at.ctx.as_bytes();
        let prefix = beat.get(..err.index()).unwrap_or(beat);
        let line = prefix.iter().filter(|ch| **ch == b'\n').count() + ctx.config_line_offset;
        ctx.handler.borrow_mut()(Diagnostic {
            level: DiagnosticLevel::Error,
            message: &err.to_string(),
            file: Some(&ctx.config_file),
            line: Some(NonZeroUsize::MIN.saturating_add(line)),
        });
        return Err(Error::JsonError(err));
    }
    Ok(())
}
