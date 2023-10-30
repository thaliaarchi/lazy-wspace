use rug::Integer;

/// A representation of the [`Show`](https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Show.html)
/// typeclass in Haskell.
pub trait Show {
    /// Converts a value to a readable string, which is a syntactically correct
    /// Haskell expression.
    fn show(&self) -> String;

    // TODO: Handle `showsPrec`.
}

// TODO: It is only wrapped in parentheses when used as an argument.
impl Show for Integer {
    fn show(&self) -> String {
        if self.is_negative() {
            format!("({self})")
        } else {
            self.to_string()
        }
    }
}
