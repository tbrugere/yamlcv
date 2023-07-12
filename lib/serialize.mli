val serialize: Yaml.value -> (Tags.tagset * Base_types.cvitem) list

(** **/** *)
(** Do not use the following functions, only exposed for unit testing **)


val product_merge: ((string * (Base_types.tags * 'a) list) list) 
    -> (Base_types.tags * ((string * 'a) list)) list
