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
    /// - `fail` in [`class MonadFail`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/Control/Monad/Fail.hs#L43-70)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#t:MonadFail))
    ///   - [`instance MonadFail IO`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/Control/Monad/Fail.hs#L83-84)
    ///     - [`failIO`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Base.hs#L1764-1765)
    ///       - [`mkUserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO.hs#L479-480)
    ///         - [`userError`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L402-411)
    ///           - [`IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L326-339)
    ///         - `toException` in [`class Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L45-144)
    ///           - [`instance Exception IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L342)
    ///           - [`SomeException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L34-39)
    ///       - [`raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/compiler/GHC/Builtin/primops.txt.pp#L2611-2618)
    ///         ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exts.html#v:raiseIO-35-))
    /// - [`instance Show IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L417-430)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Text-Show.html#t:Show))
    ///   - [`show UserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L389)
    UserError {
        /// The string passed to `fail`.
        description: String,
    },

    /// A call to `error`, which stops execution and displays an error message.
    ///
    /// # Definitions in GHC
    ///
    /// - [`error`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Err.hs#L33-40)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:error))
    ///   - [`HasCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L64-72)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:HasCallStack),
    ///     [guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/callstack.html))
    ///   - [`CallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L74-144)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:CallStack))
    ///   - [`errorCallWithCallStackException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L79-86)
    ///     - [`currentCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/CCS.hsc#L115-126)
    ///       - [`getCurrentCCS`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/CCS.hsc#L57-64)
    ///       - [`ccsToStrings`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/CCS.hsc#L128-142)
    ///     - [`prettyCallStackLines`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L113-119)
    ///       - [`getCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L146-155)
    ///     - [`showCCSStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L88-90)
    ///     - [`ErrorCallWithLocation`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L54-56)
    ///     - `toException` in [`class Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L45-144)
    ///     - [`unsafeDupablePerformIO`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Unsafe.hs#L129-145)
    ///       - [`runRW#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-prim/GHC/Magic.hs#L107-119)
    ///       - [`lazy`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-prim/GHC/Magic.hs#L64-86)
    ///   - [`raise#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/compiler/GHC/Builtin/primops.txt.pp#L2570-2585)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exts.html#v:raise-35-))
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
