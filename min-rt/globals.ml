open MiniMLRuntime;;

(**************** グローバル変数の宣言 ****************)

(* オブジェクトのデータを入れるベクトル（最大60個）*)
let objects = 
  let dummy = Array.make 0 0.0 in
  Array.make (60) 
    (0, 0, 0, 0, 
     dummy, dummy,
     false, dummy, dummy,
     dummy)

(* [| x軸の走査線本数 , y軸の走査線本数 |] *)
let size = Array.make 2 128

(* 実行時オプション: デバッグ出力の有無 *)
let dbg = Array.make 1 true
(* Screen の座標 *)
let screen = Array.make 3 0.0
(* 視点の座標 (offset なし) *)
let vp = Array.make 3 0.0
(* 視点の座標 (screen 位置分の offset あり) *)
let view = Array.make 3 0.0
(* 光源方向ベクトル (単位ベクトル) *)
let light = Array.make 3 0.0
(* スクリーンの回転方向: 三角関数の値で保持 *)
let cos_v = Array.make 2 0.0
let sin_v = Array.make 2 0.0
(* 鏡面ハイライト強度 (標準=255) *)
let beam = Array.make 1 255.0
(* AND ネットワークを保持 *)
let and_net = Array.make 50 (Array.make 1 (-1))
(* OR ネットワークを保持 *)
let or_net = Array.make 1 (Array.make 1 (and_net.(0)))

(* reader *)
let temp = Array.make 14 0.0 (* read_nth_object 内の作業変数 *)
let cs_temp = Array.make 16 0.0

(* solver *)
(**** Callee との通信用グローバル変数 ****)
(* 交点 の t の値 *)
let solver_dist = Array.make 1 0.0

(* スキャンの方向 *)
let vscan = Array.make 3 0.0
(* 交点の直方体表面での方向 *)
let intsec_rectside = Array.make 1 0
(* 発見した交点の最小の t *)
let tmin = Array.make 1 (1000000000.0)
(* 交点の座標 *)
let crashed_point = Array.make 3 0.0
(* 衝突したオブジェクト *)
let crashed_object = Array.make 1 0
(* 1つの AND ネットワークについての終了フラグ *)
let end_flag = Array.make 1 false
(* トレース開始点 *)
let viewpoint = Array.make 3 0.0
(* 法線ベクトル *)
let nvector = Array.make 3 0.0
(* スクリーン上の点の明るさ *)
let rgb = Array.make 3 0.0
(* 交点の色 *)
let texture_color = Array.make 3 0.0

(* オブジェクト中心を原点にした視点ベクトル *)
let solver_w_vec = Array.make 3 0.0

(* check_all_inside 用引数ベクトル *)
let chkinside_p = Array.make 3 0.0

(* is_outside 用内部利用 (中心差分) ベクトル *)
let isoutside_q = Array.make 3 0.0

(* グローバルに切り出したローカル変数 *)
(* nvector *)
let nvector_w = Array.make 3 0.0

(* main *)
let scan_d = Array.make 1 0.0
let scan_offset = Array.make 1 0.0
let scan_sscany = Array.make 1 0.0
let scan_met1 = Array.make 1 0.0
let wscan = Array.make 3 0.0
