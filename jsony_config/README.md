# Jsony Config

Jsony Config is an opinionated application/service configuration framework based on jsony

[![Crates.io](https://img.shields.io/crates/v/jsony_config?style=flat-square)](https://crates.io/crates/jsony_config)
[![Crates.io](https://img.shields.io/docsrs/jsony_config?style=flat-square)](https://docs.rs/jsony_config/latest/jsony_config/)
[![License](https://img.shields.io/badge/license-MIT-blue?style=flat-square)](LICENSE)

## Features

- Fast compile times.
- Lenient Json Configs Supporting
  - Trailing Commas
  - Comments
  - Unquoted static keys
- Loading from multiple config files with priority.
- Resolving paths relative to the location of the config file.
- Warnings for duplicate and unused fields, localized to the specific file and line.
- Various search strategies for finding configs.
- Lazy initialized global config, useable from tests.

## Configuration Formats

Two configuration file formats are supported: `.json` and `.js`.

### JSON (`.json`)

The `.json` format supports lenient features like trailing commas and comments, which can be useful for configuration files. However, editor support for these extensions to the JSON standard can be inconsistent and may require special configuration.

```json
// application.config.json
{
  "number": 32,
  // A useful comment
  "test_output": "./output"
}
```

### JavaScript (`.js`)

To provide a better out-of-the-box editor experience with syntax highlighting and validation for lenient JSON features, a `.js` format is also supported.
This format is a workaround that uses a subset of JavaScript syntax.

The configuration must be assigned to a `const CONFIG =` declaration. `jsony_config` will locate this line and parse the object that follows.

```javascript
// application.config.js
const CONFIG = {
  number: 32,
  // A useful comment
  test_output: "./output",
};
```

**Note:** While this looks like JavaScript, it is not executed as such. JavaScript features like variables, functions, or arithmetic are not supported.
This approach is simply a "hack" to leverage editor support for JavaScript object literals, which closely resembles the lenient JSON syntax.

### Example

```rust
use jsony_config::{Search, GlobalConfig, relative_path};

#[derive(jsony::Jsony, Debug)]
#[jsony(Flattenable)]
pub struct Config {
   #[jsony(default = 42)]
   number: u32,
   #[jsony(with = relative_path)]
   test_output: Option<std::path::PathBuf>,
}

static CONFIG: GlobalConfig<Config> = GlobalConfig::new(&[
    Search::Flag("--config"),
    Search::Upwards{
        file_stem: "application.config",
        override_file_stem: Some("application.local.config"),
    },
]);

fn main() {
    CONFIG.initialize(&mut jsony_config::print_diagnostics).unwrap();
    println!("{:#?}", CONFIG);
    assert_eq!(CONFIG.number, 42)
}
```
