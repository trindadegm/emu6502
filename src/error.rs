/* *****************************************************************************
   Copyright 2020 Gustavo Moitinho Trindade

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
***************************************************************************** */
// src/error.rs
use std::{error, fmt};

/// An error type to be used by the Glez crate. Can be created as a standalone, with an
/// `ErrorClass` and a description (`str`).
#[derive(Debug)]
pub struct Error {
    class: ErrorClass,
    description: String,
    // This error may or may not have been created from some other error (as a source)
    source: Option<Box<dyn error::Error>>,
}

/// An error class is used to give a general idea of the error. Most of the classes are very
/// general and depend on the context to actually mean something. For this reason, the `Error`
/// struct contains a textual description of the error alongside its class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorClass {
    /// Errors caused when a search can't find an object or resource.
    NotFound,
    /// Errors caused by operations that take longer too long.
    Timeout,

    /// Errors caused when a resource is unavailable.
    Unavailable,
    /// Errors caused by expired resources.
    Expired,
    /// Errors caused by locked resources.
    Locked,

    /// Errors caused by using arguments that do not comply with the expected format.
    InvalidArgument,
    /// Errors caused when accessing elements out of bounds.
    OutOfBounds,

    /// Errors involving failing to allocate host memory.
    OutOfHostMem,
    /// Errors involving failing to allocate device memory.
    OutOfDeviceMem,

    /// Errors caused when software, hardware or data formats are not supported.
    Unsupported,

    /// Errors involving calculation errors, overflows or underflows.
    NumericError,
    /// Errors involving parsing strings or some other data.
    ParseError,

    /// General runtime error. Used when the problem is outside of the program control.
    RuntimeError,
    /// General logic error. Used when the problem is on the programming logic (a bug). Any error
    /// that cannot be categorized as one of the other classes should fit into here.
    LogicError,
}

impl Error {
    /// Creates a new `Error`, of class `ErrorClass` and with a description.
    pub fn new<S>(class: ErrorClass, description: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            class,
            description: description.into(),
            source: None,
        }
    }

    /// Creates a new `Error`, of class `ErrorClass` with a description and a source error.
    pub fn with_source<E, S>(class: ErrorClass, description: S, source: E) -> Self
    where
        E: error::Error + 'static,
        S: Into<String>,
    {
        Self {
            class,
            description: description.into(),
            source: Some(Box::new(source)),
        }
    }

    /// Retrieves the error class.
    #[inline]
    pub fn class(&self) -> ErrorClass {
        self.class
    }

    /// Retrieves the error description as a `&str`.
    #[inline]
    pub fn description(&self) -> &str {
        &self.description
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}, {}", self.class, self.description)
    }
}

impl error::Error for Error {
    /// Returns `Some` with the error source if it exists. `None` otherwise.
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        if let Some(ref box_source) = self.source {
            Some(box_source.as_ref())
        } else {
            None
        }
    }
}
