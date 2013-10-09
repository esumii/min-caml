let _ =
  let img1 = Ppm.load Sys.argv.(1) in
  let img2 = Ppm.load Sys.argv.(2) in
  let w = Ppm.width img1 in
  let h = Ppm.height img1 in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      for k = 0 to 2 do
        let i1 = Ppm.get img1 i j k in
        let i2 = Ppm.get img2 i j k in
        let d = 128 + i1 - i2 in
        Ppm.set img1 i j k d
      done
    done
  done;
  Ppm.dump Sys.argv.(3) img1
