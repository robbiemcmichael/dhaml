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

## Usage

Start with an input file containing a Dhall value:

```bash
$ cat input.dhall
```
```dhall
{ string = "foo"
, number = 2
}
```

Use the `!dhall` tag to embed Dhall expressions. The input expression from
`input.dhall` will be bound to the variable `x` which may then be used in
subsequent expressions:

```bash
$ cat template.yaml
```
```yaml
data:
  string: !dhall x.name
  number: !dhall 10 * x.number + 5
  record: !dhall |
    { a = "abc"
    , b = True
    }
```

Evaluate all Dhall expressions in the YAML file with `dhaml`:

```bash
$ dhaml template.yaml <<< ./input.dhall
```
```yaml
config:
  string: foo
  number: 35
  record:
    a: abc
    b: true
```

[dhall]: https://dhall-lang.org
[tag]: https://yaml.org/spec/1.2/spec.html#id2761292
