(*
   A graphical frontend (handles backend notifications,
   copy_in, copy_out, presentation of "select" result in tables)

   To build prompt_gtk you need lablgtk 1.2
*)

open Printf
open GMain
open! Postgresql

let () =
  if (Array.length Sys.argv <> 2) then (
    eprintf "Usage:  %s conninfo\n" Sys.argv.(0);
    exit 1)

let conninfo = Sys.argv.(1)

let file_dialog title =
  let name = ref "" in
  let sel = GWindow.file_selection ~title ~modal:true () in
  let cancel_callback () = sel#destroy (); Main.quit () in
  let ok_callback () = name := sel#filename; cancel_callback () in
  let _ = sel#ok_button#connect#clicked ~callback:ok_callback in
  let _ = sel#cancel_button#connect#clicked ~callback:cancel_callback in
  sel#show ();
  Main.main ();
  !name

let make_window title =
  let window = GWindow.window ~title ~width:300 ~height:300 () in
  let vbox = GPack.vbox ~packing:window#add () in

  let button =
    GButton.button ~label:"Close" ~packing:(fun widget ->
      vbox#pack ~from:`END widget) () in

  let _ = button#connect#clicked ~callback:window#destroy in

  let hbox = GPack.hbox ~packing:vbox#add () in
  let sbv = GRange.scrollbar `VERTICAL ~packing:(hbox#pack ~from:`END) () in
  let sbh = GRange.scrollbar `HORIZONTAL ~packing:(vbox#pack ~from:`END) () in

  window, hbox, sbv, sbh

let show_tuples res =
  let window, hbox, sbv, sbh = make_window "Result (tuples)" in

  let cl =
    GList.clist
      ~titles:res#get_fnames_lst
      ~shadow_type:`OUT
      ~packing:hbox#add
      ~vadjustment:sbv#adjustment
      ~hadjustment:sbh#adjustment
      () in

  for tuple = 0 to res#ntuples - 1 do
    ignore (cl#append (res#get_tuple_lst tuple))
  done;

  cl#columns_autosize ();
  window#show ()

let show_copy_out conn =
  let window, hbox, _sbv, _sbh = make_window "Result (copy_out)" in
  let txt = GText.view ~packing:hbox#add () in
  let buf = txt#buffer in
  conn#copy_out (fun s -> buf#insert (s ^ "\n"));
  window#show ()

let main () =
  let conn = new connection ~conninfo () in

  let window = GWindow.window ~title:"Queries" ~width:300 ~height:300 () in
  let _ = window#connect#destroy ~callback:Main.quit in
  let vbox = GPack.vbox ~border_width:5 ~spacing:10 ~packing:window#add () in
  let result = GText.view ~editable:false ~packing:vbox#add () in
  let res_buf = result#buffer in
  let text = GText.view ~editable:true ~packing:vbox#add ~height:50 () in
  let print s = ignore (res_buf#insert s) in

  let rec dump_res () =
    match conn#get_result with
    | Some res ->
        (match res#status with
        | Tuples_ok | Single_tuple -> show_tuples res
        | Copy_out -> show_copy_out conn
        | Copy_both -> show_copy_out conn
        | Copy_in ->
            let name = file_dialog "Choose file to send" in
            if name = "" then (conn # putline "\\.\n"; conn#endcopy)
            else (
              let ic = open_in name in
              conn#copy_in_channel ic;
              close_in ic)
        | Empty_query -> print "Empty query\n"
        | Command_ok -> print (sprintf "Command ok [%s]\n" res#cmd_status)
        | Bad_response ->
            print (sprintf "Bad response : %s\n" res#error); conn#reset
        | Nonfatal_error -> print (sprintf "Non fatal error : %s\n" res#error)
        | Fatal_error -> print (sprintf "Fatal error : %s\n" res#error));
        dump_res ()
    | None -> () in

  let query () =
    let buf = text#buffer in
    let s = buf#get_text () in
    print "-> "; print s; print "\n";
    buf#delete ~start:buf#start_iter ~stop:buf#end_iter;
    conn#send_query s;
    dump_res ();
    print "======\n";
    flush stdout in

  let key_press k =
    if GdkEvent.Key.keyval k = GdkKeysyms._KP_Enter then (query (); true)
    else false in

  let _ = text#event#connect#key_press ~callback:key_press in
  let button = GButton.button ~label:"Exec" ~packing:vbox#add () in
  let _ = button#connect#clicked ~callback:query in

  window#show ();

  let window =
    GWindow.window ~title:"Backend notifications" ~width:300 ~height:150 () in

  let _ = window#connect#destroy ~callback:Main.quit in
  let vbox = GPack.vbox ~border_width:5 ~packing:window#add () in
  let hbox = GPack.hbox ~packing:vbox#add () in
  let sb = GRange.scrollbar `VERTICAL ~packing:(hbox#pack ~from:`END) () in

  let clist =
    GList.clist
      ~titles:["Backend PID"; "Notification"]
      ~shadow_type:`OUT
      ~packing:hbox#add
      ~vadjustment:sb#adjustment
      () in

  let hbox = GPack.hbox ~packing:vbox#pack () in
  let button_clear = GButton.button ~label:"Clear" ~packing:hbox#add () in
  let _ = button_clear#connect#clicked ~callback:clist#clear in
  let button_clear = GButton.button ~label:"Hide" ~packing:hbox#add () in
  let _ = button_clear#connect#clicked ~callback:window#misc#hide in

  let rec dump_notification () =
    match conn#notifies with
    | Some { Notification.name; pid; extra } ->
        let _ = clist#append [string_of_int pid; name; extra] in
        window#show ();
        dump_notification ()
    | None -> () in

  let _ =
    Timeout.add ~ms:100
      ~callback:(fun () -> conn#consume_input; dump_notification (); true) in

  Main.main ()

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
