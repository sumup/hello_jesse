# hello\_jesse

__hello_jesse__ is [JSON Schema][1] (Draft 03) to [Hello][2] JSON-RPC
parameter validation functions compiler. It uses [jesse][3] for validations
against the schema.
The project also includes [rebar][4] plugin for compiling schema files.

  [Travis-CI](http://travis-ci.org/brb/hello_jesse) :: ![Travis-CI](https://secure.travis-ci.org/brb/hello_jesse.png)

## Dependencies

* [rebar][4]
* [jiffy][5]
* [jesse][3]

## Usage

First of all, a valid schema file with __.schema__ extension should be created.
The file provides multiple schemas wrapped into array. Each schema must
contain __"title"__ property which corresponds to JSON-RPC method name.

```javascript
// "math.schema"
[
  {
    "title" : "subtract",
    "type" : "object",
    "properties" : {
      "subtrahend" : {
        "type" : "number"
      },
      "minuend" : {
        "type" : "number"
      }
    },
    "required" : ["subtrahend", "minuend"]
  },
  {
    "title" : "add",
    "type" : "object",
    "properties" : {
      "addends" : {
        "type" : "array",
        "items" : {
          "type" : "number"
        }
      }
    }
  }
]
```

A schema file can be compiled with either `hello_jesse:compile(PathToSchema,
Options)` or `hello_jesse:compile_dir(SchemasDir, Options)`. In both cases,
`Options` is a list which can contain `{outdir, Dir :: file:filename()}` option
denoting a target dir (default is _"ebin"_).

The compiled schema is being written to _hj_SCHEMA_NAME_schema_ module file.
The module exports a function per method which can be included directly into
[Hello][2] `param_info/1` callback function.

```erlang
param_info(subtract)  -> hj_math_schema:subtract().
param_info(add)       -> hj_math_schema:add().
```

## Rebar plugin

Compilation can be done by `rebar compile` if the following line is included in
rebar.config:

```erlang
{plugins, [hello_jesse_plugin]}
```

The following config parameters can be specified:

```erlang
{hello_jesse_plugin, [
  {src, SchemaSrcDir},
  {outdir, OutDir}
]}
```

## Notes

Currently, parameters as array are not supported. In that case,
`{error, <<"unsuported_params_as_array">>}` is returned.


[1]: http://tools.ietf.org/html/draft-zyp-json-schema-03
[2]: https://github.com/fjl/hello
[3]: https://github.com/klarna/jesse
[4]: https://github.com/rebar/rebar
[5]: https://github.com/davisp/jiffy
