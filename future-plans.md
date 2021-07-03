# Future plans

### Compile-time type-checking

Partially implemented.

Some things are checked and others aren't (mainly classes/objects).

Some builtin types can optionally take type parameters, which are erased at runtime.
Other types are currently unable to take type parameters.

All Neko builtins (`$<name>`) are typed to some degree, with select functions like `$apply`, `$closure`, and `$array` having special/generic behavior.
Other functions and methods are currently unable to take type parameters.

Basic flow-typing is available is via `switch $typeof(myVar) {...}`.


### Pattern matching


### Better standard library


### Modules/namespaces


### Protocols


### Code examples