-- | WARNING: All the definitions in these \"Internal\" modules are not version-controlled.
-- If you depend on them, your code may break even when the nanopass package version does not increase.
--
-- They are here because I have found it useful to generate documentation for how this library works.
-- Developers can then check on how the internal interfaces are meant to work together.
--
-- * "Nanopass.Internal.Representation" defines an intermediate representation that is at the core of all operations.
-- * "Nanopass.Internal.Parser" is responsible for parsing quasiquotes into language definitions and modifications.
module Nanopass.Internal () where