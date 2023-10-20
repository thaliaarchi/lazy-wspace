use crate::hs;

/// A representation of Haskell errors and exceptions.
///
/// It does not attempt to represent all Haskell errors and exceptions—only
/// those that may be encountered in the execution of a Whitespace program by
/// the reference interpreter.
///
/// In Haskell, [errors and exceptions](https://wiki.haskell.org/Error_vs._Exception)
/// are distinct. Exceptions can be manipulated as values and signify
/// recoverable situations, while errors are for programming errors and abort
/// the program. In the reference interpreter, however, no exceptions or errors
/// are caught, so they act functionally the same, aborting the program, and are
/// combined in this type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    /// A user-created exception from the `IO` monad.
    ///
    /// Specifically, it is an `IOError` constructed with the `fail` method of
    /// the `MonadFail` instance for `IO`.
    ///
    /// # Definitions in GHC
    ///
    /// - `fail` in [`class MonadFail`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/Control/Monad/Fail.hs#L43-70)
    ///   - [`instance MonadFail IO`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/Control/Monad/Fail.hs#L83-84)
    ///     - [`failIO`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Base.hs#L1905-1906)
    ///       - [`mkUserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO.hs#L484-485)
    ///         - [`userError`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L410-419)
    ///           - [`IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L331-347)
    ///         - `toException` in [`class Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception/Type.hs#L136-149)
    ///           - [`instance Exception IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L350)
    ///           - [`SomeException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception/Type.hs#L39-44)
    ///       - [`raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp#L2690-2698)
    /// - [`instance Show IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L425-438)
    ///   - [`show UserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/IO/Exception.hs#L397)
    UserError {
        /// The string passed to `fail`.
        description: String,
    },

    /// A call to `error`, which stops execution and displays an error message.
    ///
    /// # Definitions in GHC
    ///
    /// - [`error`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Err.hs#L33-40)
    ///   - [`HasCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Stack/Types.hs#L69-77)
    ///     ([docs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/callstack.html))
    ///   - [`CallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Stack/Types.hs#L79-149)
    ///   - [`errorCallWithCallStackException`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception.hs#L101-108)
    ///     - [`currentCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Stack/CCS.hsc#L115-126)
    ///       - [`getCurrentCCS`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Stack/CCS.hsc#L57-64)
    ///       - [`ccsToStrings`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Stack/CCS.hsc#L128-142)
    ///     - [`prettyCallStackLines`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception.hs#L135-141)
    ///       - [`getCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Stack/Types.hs#L151-160)
    ///     - [`showCCSStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception.hs#L110-112)
    ///     - [`ErrorCallWithLocation`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception.hs#L76-78)
    ///     - `toException` in [`class Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/src/GHC/Exception/Type.hs#L136-149)
    ///   - [`raise#`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp#L2651-2661)
    Error {
        /// The string passed to `error`.
        description: String,
        /// The location stored in `ErrorCallWithLocation`.
        location: String,
    },
}

impl hs::Show for Error {
    fn show(&self) -> String {
        match self {
            Error::UserError { description } => {
                if description.is_empty() {
                    "user error".to_string()
                } else {
                    format!("user error ({description})")
                }
            }
            Error::Error {
                description: _,
                location: _,
            } => todo!(),
        }
    }
}
