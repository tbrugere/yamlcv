exception SerializeError

open Yamlcv
open Yamlcv.Base_types





let main input_file output_file = 
    let file_content = 
        match input_file with
        | "-" -> CCIO.read_all stdin
        | _ -> CCIO.(with_in input_file read_all) in
    let yaml = Yaml.of_string_exn file_content in
    let items = try serialize yaml with Failure s -> CCIO.write_line stderr s; raise SerializeError
    in
    let lines = 
        items 
        |> List.map 
        (fun (tags, cvitem) ->  
                [%string "%{Tags.tagset_to_string tags} %{show_cvitem cvitem}"])
    in 
    match output_file with
    | "-" -> CCIO.write_lines_l stdout lines
    | _ -> CCIO.(with_out output_file CCIO.write_lines_l lines)

let cmd = 
    let open Cmdliner in
    let doc = "TODO" in
    let input_arg = Arg.(value & opt string "-" & info ["i"; "input"] ~docv:"FILENAME" ~doc) in 
    let output_arg = Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"FILENAME" ~doc) in
    let term = Term.(const main $ input_arg $ output_arg) in
    let info = Cmd.info "yamlcv" ~doc in
    Cmd.v info term

let () = exit Cmdliner.(Cmd.eval cmd)

