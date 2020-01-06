# Yaml data munging

source: <https://ro-che.info/articles/2015-07-26-better-yaml-parsing>

That parser works in two stages.

During the first stage, it parses YAML into a generic representation, such as an array of dictionaries of strings. For this, the yaml package uses the libyaml C library written by Kirill Simonov.

During the second stage, the generic representation is converted into the application-specific Haskell type. For instance, an abstract dictionary may be mapped to a record type.

This idea of two-stage parsing is borrowed from the aeson package, which parses JSON in a similar way. And because JSON’s and YAML’s data models are similar, the yaml package borrows from Aeson not only the above idea but also the generic representation and the machinery to convert it to Haskell types.

## Config Yaml Parser Example

source: <https://github.com/snoyberg/yaml/blob/master/yaml/examples/Config.hs>
