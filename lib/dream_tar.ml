open Lwt.Infix

module Writer = struct
  type out_channel = Dream.response
  type 'a t = 'a Lwt.t
  let really_write response cs =
    Dream.write response (Cstruct.to_string cs)
end

module HW = Tar.HeaderWriter(Lwt)(Writer)

let write_block (header : Tar.Header.t) lpath response =
  HW.write ~level:Tar.Header.Ustar header response >>= fun () ->
  Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string lpath) >>= fun ic ->
  let buf_len = 4 * 1024 * 1024 in
  let buf = Bytes.create buf_len in
  let rec loop () =
    Lwt_io.read_into ic buf 0 buf_len >>= fun r ->
    if r = 0 then
      Lwt.return_unit
    else
      Dream.write response (Bytes.sub_string buf 0 r) >>= fun () ->
      loop ()
  in
  loop () >>= fun () ->
  Dream.write_buffer response (Cstruct.to_bigarray (Tar.Header.zero_padding header))

let header_of_file mod_time (file : Builder_db.file) =
  let file_mode = if Fpath.is_prefix Fpath.(v "bin/") file.filepath then
      0o755
    else
      0o644
  in
  Tar.Header.make ~file_mode ~mod_time (Fpath.to_string file.filepath) (Int64.of_int file.size)

let tar_response datadir finish (files : Builder_db.file list) (response : Dream.response) =
  Lwt_list.iter_s (fun file ->
      let hdr = header_of_file finish file in
      write_block hdr Fpath.(datadir // file.localpath) response)
    files >>= fun () ->
  Writer.really_write response Tar.Header.zero_block >>= fun () ->
  Writer.really_write response Tar.Header.zero_block >>= fun () ->
  Dream.close_stream response
