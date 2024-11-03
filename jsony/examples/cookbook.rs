use jsony::{Jsony, LazyValue};

/// The `LazyValue` type can be used defer parsing parts
/// of a payload until later.
fn defered_parsing() -> Res<()> {
    #[derive(Jsony, Debug)]
    #[jsony(Json)]
    struct Alpha<'a> {
        value: &'a str,
        lazy: &'a LazyValue,
    }

    let input = jsony::object! {
        value: "Hello",
        lazy: {
            key: true,
            numbers: [1, 2, 3],
            nested: {
                data: 42
            }
        }
    };

    let alpha: Alpha = jsony::from_json(&input)?;

    assert_eq!(alpha.lazy["nested"]["data"].parse::<u32>()?, 42);

    let output = jsony::to_json(&alpha);

    assert_eq!(input, output.as_str());

    Ok(())
}

fn main() {
    defered_parsing().unwrap();
}

struct Error {
    location: &'static std::panic::Location<'static>,
    error: Box<dyn std::error::Error>,
}
type Res<T> = Result<T, Error>;

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed @ {}: {:?}", self.location, self.error)
    }
}

impl<E> From<E> for Error
where
    E: Into<Box<dyn std::error::Error>>,
{
    #[track_caller]
    #[inline]
    fn from(value: E) -> Self {
        Self {
            location: &std::panic::Location::caller(),
            error: value.into(),
        }
    }
}
