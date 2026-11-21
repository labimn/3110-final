open Lwt
open Js_of_ocaml

let main () : unit Lwt.t =
  let message = Dom_html.getElementById "app" in
  message##.textContent := Js.some (Js.string "Welcome to SpendWise!");
  Lwt.return ()

let () = Lwt.async main