let () =
  if (Sys.command("which qtest > /dev/null") != 0) {
    ();
      /* create empty file */
  } else {
    let files = "../src/OSeq.ml ../src/OSeq.mli";
    let cmd = Printf.sprintf("qtest extract %s 2>/dev/null", files);

    exit(Sys.command(cmd));
  };
