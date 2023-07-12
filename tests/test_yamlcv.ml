open Yamlcv
open Base_types

let tagset_0 = Tags.tagset_of_list []
let tagset_1 = Tags.tagset_of_list ["tag1"]
let tagset_2 = Tags.tagset_of_list ["tag2"]
let tagset_1no2 = Tags.tagset_of_list ["tag1"; "no-tag2"]
let tagset_2no1 = Tags.tagset_of_list ["tag2"; "no-tag1"]

let tags_testable = Alcotest.testable Tags.pp_tagset (Tags.TagMap.equal (=))

let product_merge_test_cases:
        ( string * (string * (tags * string) list) list * ((tags * ((string * string) list)) list)) list
        = 
        [
            "12no-negative-stuff", 
            [
                ("field0", [tagset_0, "value0"]);
                ("field1", [tagset_1, "value1"; tagset_2, "value2"]);
            ]
            , 
            [
                (tagset_1, [("field0", "value0"); ("field1", "value1")]);
                (tagset_2, [("field0", "value0"); ("field1", "value2")]);
            ]
        ;
            "12-with-negatives", 
            [
                ("field0", [tagset_0, "value0"]);
                ("field1", [tagset_1no2, "value1"; tagset_2no1, "value2"]);
            ]
            , 
            [
                (tagset_1no2, [("field0", "value0"); ("field1", "value1")]);
                (tagset_2no1, [("field0", "value0"); ("field1", "value2")]);
            ]
        ;
            "22-with-negatives", 
            [
                ("field0", [tagset_1no2, "valuea"; tagset_2no1, "valueb"]);
                ("field1", [tagset_1no2, "value1"; tagset_2no1, "value2"]);
            ]
            , 
            [
                (tagset_1no2, [("field0", "valuea"); ("field1", "value1")]);
                (tagset_2no1, [("field0", "valueb"); ("field1", "value2")]);
            ]
        ]

let test_product_merge input expected () = 
    let actual = Yamlcv.Serialize.product_merge input in
    Alcotest.(check' (list (pair tags_testable (list (pair string string))))) 
        ~msg:"product_merge"
        ~expected ~actual

let test_file input_file () = 
    let file_content = 
        match input_file with
        | "-" -> CCIO.read_all stdin
        | _ -> CCIO.(with_in input_file read_all) 
    in
    let yaml = Yaml.of_string_exn file_content in
    let parsed = Yamlcv.Serialize.serialize yaml in
    ignore parsed;;
ignore test_file;;

let () = 
    let open Alcotest in 
    run "Yamlcv" [
        "product_merge", List.map 
         (fun (name, input, expected) -> 
             test_case name `Quick (test_product_merge input expected)) 
         product_merge_test_cases;
    ] |> ignore;
