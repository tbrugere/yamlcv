module Tags = Tags
module Html = Html
module Base_types = Base_types
module Serialize = Serialize


val serialize: Yaml.value -> (Tags.tagset * Base_types.cvitem) list


