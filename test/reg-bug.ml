let rec h p = 
    let (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) = p in
    let rec g z = 
        let r = v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 in
        if z > 0 then r else g (-z) in
    g 1
in 
print_int (h (1,2,3,4,5,6,7,8,9,10));
print_newline ()
(* 
min-caml generates the following virtual code for g:
>>>
g.52 [z.53; ] []
  v9.50 := Ld %edi 40 1
  v8.49 := Ld %edi 36 1
  v7.48 := Ld %edi 32 1
  v6.47 := Ld %edi 28 1
  v5.46 := Ld %edi 24 1
  v4.45 := Ld %edi 20 1
  v3.44 := Ld %edi 16 1
  v2.43 := Ld %edi 12 1
  v10.51 := Ld %edi 8 1
  v1.42 := Ld %edi 4 1
  Ti16.65 := Add v1.42 v2.43
  Ti17.64 := Add Ti16.65 v3.44
  Ti18.63 := Add Ti17.64 v4.45
  Ti19.62 := Add Ti18.63 v5.46
  Ti20.61 := Add Ti19.62 v6.47
  Ti21.60 := Add Ti20.61 v7.48
  Ti22.59 := Add Ti21.60 v8.49
  Ti23.58 := Add Ti22.59 v9.50
  r.55 := Add Ti23.58 v10.51
  IfLE z.53 0
  Then
    Ti25.57 := Neg z.53
    CallCls g.52 [Ti25.57; ] []
  Else
    Mov r.55
<<<

In this calling conventions, %edi is used as the closure register.
And after register allocation, g would be:
>>>
g.52 [%eax; ] []
  %ebx := Ld %edi 40 1
  %ecx := Ld %edi 36 1
  %edx := Ld %edi 32 1
  %esi := Ld %edi 28 1
  Tu94 := Save %edi g.52
  %edi := Ld %edi 24 1      // %edi is unintentionally replaced!!
  Tu93 := Save %eax z.53
  %eax := Ld %edi 20 1
  Tu92 := Save %ebx v9.50
  %ebx := Ld %edi 16 1
  Tu91 := Save %ecx v8.49
  %ecx := Ld %edi 12 1
  Tu90 := Save %edx v7.48
  %edx := Ld %edi 8 1
  Tu89 := Save %edx v10.51
  %edx := Ld %edi 4 1
  %edx := Add %edx %ecx
  %edx := Add %edx %ebx
  %edx := Add %edx %eax
  %edx := Add %edx %edi
  %edx := Add %edx %esi
  %eax := Restore v7.48
  %edx := Add %edx %eax
  %eax := Restore v8.49
  %edx := Add %edx %eax
  %eax := Restore v9.50
  %edx := Add %edx %eax
  %eax := Restore v10.51
  %eax := Add %edx %eax
  %ebx := Restore z.53
  IfLE %ebx 0
  Then
    %eax := Neg %ebx
    %edi := Restore g.52
    CallCls %edi [%eax; ] []
  Else
    Mov %eax
<<<
*)

