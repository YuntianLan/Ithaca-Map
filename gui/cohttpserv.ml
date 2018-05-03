open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    let _ = print_endline (uri) in
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
        (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
           uri meth headers body))
    >>= (fun body -> Server.respond_string ~headers:(Header.init_with "Access-Control-Allow-Origin" "*") ~status:`OK ~body:"hello world" ())
    (* (body |> Cohttp_lwt.Body.to_string) >>= 
       (fun body -> Server.respond_string ~status:`OK ~body:"hello world" ()) *)
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)