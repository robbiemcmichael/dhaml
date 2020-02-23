# dhaml

`dhaml` is a better alternative to YAML templating. It allows [Dhall][dhall]
expressions to be embedded within YAML documents using the `!dhall` [tag][tag]
and it will always substitute the expression with another valid YAML value.
This has several benefits over traditional YAML templating approaches:

- **Resistant to injection attacks** - Dhall expressions are always substituted
  with a valid YAML value or the program will return a non-zero exit code.
  There's also no need to manually escape or indent the output of an
  expression.
- **Type safety** - Dhall expressions are statically typed. This allows type
  annotations to be used to ensure that values match the expected type.
- **Import resolution** - Dhall expressions can be imported from local files or
  from a URL. This allows complex expressions to be moved into dedicated
  `.dhall` files or turned into a library.

## Releases

- Statically linked executables are available from the [releases][releases] page
- Docker images are published to [`robbiemcmichael/dhaml`][docker-hub] on
  Docker Hub

The Docker release is a scratch image containing only the executable. It will
likely be more useful to copy the executable into a new Docker image along with
any other tools needed, for example:

```dockerfile
FROM alpine:latest

COPY --from=dhallhaskell/dhall:1.30.0 /bin/dhall /bin/
COPY --from=robbiemcmichael/dhaml:0.3.0 /bin/dhaml /bin/
```

## Usage

```
Usage: dhaml FILE [-i|--input]

Available options:
  FILE                     YAML file containing Dhall expressions
  -i,--input               Bind the expression from stdin to x
  -h,--help                Show this help text
```

### Worked example

Start with an input file containing a Dhall value:

```bash
$ cat input.dhall
```
```dhall
{ string = "foo"
, number = 2
, function = \(x : Natural) -> x*x : Natural
}
```

Use the `!dhall` tag to embed Dhall expressions. The input expression from
`input.dhall` will be bound to the variable `x` which may then be used in
subsequent expressions:

```bash
$ cat template.yaml
```
```yaml
config:
  string: !dhall x.string
  number: !dhall x.function (x.number + 2)
  record: !dhall |
    { a = "abc"
    , b = True
    }
  source: !dhall ./input.dhall as Text
```

Evaluate all Dhall expressions in the YAML file with `dhaml`:

```bash
$ dhaml template.yaml -i <<< ./input.dhall
```
```yaml
config:
  string: foo
  number: 16
  record:
    a: abc
    b: true
  source: |
    { string = "foo"
    , number = 2
    , function = \(x : Natural) -> x*x : Natural
    }
```

[dhall]: https://dhall-lang.org
[tag]: https://yaml.org/spec/1.2/spec.html#id2761292
[releases]: https://github.com/robbiemcmichael/dhaml/releases
[docker-hub]: https://hub.docker.com/r/robbiemcmichael/dhaml
