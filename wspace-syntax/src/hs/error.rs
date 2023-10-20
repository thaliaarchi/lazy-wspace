use crate::hs;

/// A representation of Haskell [`Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception/Type.hs#L136-149)
/// kinds.
///
/// It does not attempt to represent all Haskell exceptions—only those that may
/// be encountered in the execution of a Whitespace program by the reference
/// interpreter.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exception {
    /// An `IOError` constructed with the `fail` method of the `MonadFail`
    /// instance for `IO`.
    ///
    /// # Implementation in GHC
    ///
    /// Simplified definition:
    ///
    /// ```haskell
    /// instance MonadFail IO where
    ///     fail = failIO
    ///
    /// failIO :: String -> IO a
    /// failIO str = IO (raiseIO# (SomeException (userError str)))
    ///
    /// userError :: String -> IOError
    /// userError str = IOError Nothing UserError "" str Nothing Nothing
    ///
    /// -- Primitive operation
    /// raiseIO# :: a -> State# RealWorld -> (# State# RealWorld, b #)
    /// ```
    ///
    /// Definitions in GHC:
    /// - [`class MonadFail`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/Control/Monad/Fail.hs#L43-70)
    ///   - [`instance MonadFail IO`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/Control/Monad/Fail.hs#L83-84)
    ///     - [`failIO`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Base.hs#L1905-1906)
    ///       - [`mkUserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO.hs#L484-485)
    ///         - [`userError`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L410-419)
    ///           - [`IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L335-347)
    ///         - [`class Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception/Type.hs#L136-149)
    ///           - [`instance Exception IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L350)
    ///           - [`SomeException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception/Type.hs#L39-44)
    ///       - [`raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp#L2690-2698)
    /// - [`instance Show IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L425-438)
    ///   - [`show UserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L397)
    UserError { description: String },
}

impl hs::Show for Exception {
    fn show(&self) -> String {
        match self {
            Exception::UserError { description } => {
                if description.is_empty() {
                    "user error".to_string()
                } else {
                    format!("user error ({description})")
                }
            }
        }
    }
}
