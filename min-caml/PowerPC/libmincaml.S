	.cstring
	.align 	2
LC0:
	.ascii	"%d\0"
	.align	2
LC1:
	.ascii 	"%lf\0"
	.literal8
	.align 	3
LC2:
	.long	1127219200
	.long	-2147483648
	.text
	.align	2
	.globl 	min_caml_print_newline
min_caml_print_newline:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu  	r1, -80(r1)
	mr 	r30, r1
	li	r3, 10
	bl 	putchar
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
putchar:
	.indirect_symbol _putchar
	mflr 	r0
	bcl 	20, 31, L1spb
L1spb:
	mflr 	r11
	addis 	r11, r11, ha16(putchar_lazy-L1spb)
	mtlr 	r0
	lwzu 	r12, lo16(putchar_lazy-L1spb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
putchar_lazy:
	.indirect_symbol _putchar
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
#	print_int
	.text
	.align 	2
	.globl	min_caml_print_int
min_caml_print_int:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -80(r1)
	mr 	r30, r1
	bcl 	20, 31, L2pb
L2pb:
	mflr 	r31
	mr	r4, r2
	addis 	r2, r31, ha16(LC0 - L2pb)
	la 	r3, lo16(LC0 - L2pb)(r2)
	bl	printf
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
printf:
	.indirect_symbol _printf$LDBLStub
	mflr 	r0
	bcl 	20, 31, L2spb
L2spb:
	mflr 	r11
	addis 	r11, r11, ha16(printf_lazy-L2spb)
	mtlr 	r0
	lwzu 	r12, lo16(printf_lazy-L2spb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
printf_lazy:
	.indirect_symbol _printf$LDBLStub
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
#	print_byte
	.text
	.align 	2
	.globl	min_caml_print_byte
min_caml_print_byte:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu  	r1, -80(r1)
	mr 	r30, r1
	stw	r3, 104(r30)
	mr	r3, r2
	bl 	putchar
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr	
#	prerr_int
	.text
	.align 	2
	.globl 	min_caml_prerr_int
min_caml_prerr_int:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -80(r1)
	mr 	r30, r1
	bcl 	20, 31, L3pb
L3pb:
	mflr 	r31
	mr	r6, r2
	mr	r2, r5
	mr	r5, r6
	addis 	r2, r31, ha16(L - L3pb)
	lwz 	r2, lo16(L - L3pb)(r2)
	addi 	r0, r2, 176
	mr 	r3, r0
	addis 	r2, r31, ha16(LC0 - L3pb)
	la 	r4, lo16(LC0 - L3pb)(r2)
	bl 	fprintf
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.non_lazy_symbol_pointer
L:
	.indirect_symbol ___sF
	.long	0
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
fprintf:	
	.indirect_symbol _fprintf$LDBLStub
	mflr 	r0
	bcl 	20, 31, L3spb
L3spb:
	mflr 	r11
	addis 	r11, r11, ha16(fprintf_lazy - L3spb)
	mtlr 	r0
	lwzu 	r12, lo16(fprintf_lazy - L3spb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
fprintf_lazy:
	.indirect_symbol _fprintf$LDBLStub
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
#	prerr_byte
	.text
	.align 	2
	.globl	min_caml_prerr_byte
min_caml_prerr_byte:
	mflr	r0
	stmw	r30, -8(r1)
	stw	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu	r1, -80(r1)
	mr	r30, r1
	bcl	20, 31, L4pb
L4pb:
	mflr	r31
	mr	r3, r2
	addis 	r2, r31, ha16(L - L4pb)
	lwz	r2, lo16(L - L4pb)(r2)
	addi	r0, r2, 176
	mr	r4, r0
	bl	fputc
	lwz	r1, 0(r1)
	lwz	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr	r0
	lmw	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
fputc:
	.indirect_symbol _fputc
	mflr	r0
	bcl	20, 31, L4spb
L4spb:
	mflr	r11
	addis	r11, r11, ha16(fputc_lazy - L4spb)
	mtlr	r0
	lwzu	r12, lo16(fputc_lazy - L4spb)(r11)
	mtctr	r12
	bctr
	.lazy_symbol_pointer
fputc_lazy:
	.indirect_symbol _fputc
	.long	dyld_stub_binding_helper
	.subsections_via_symbols	
#	prerr_float
	.text
	.align 	2
	.globl 	min_caml_prerr_float
min_caml_prerr_float:
	mflr 	r0
	stmw 	r29, -12(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -96(r1)
	mr 	r30, r1
	bcl 	20, 31, L5pb
L5pb:
	mflr 	r31
	addis 	r2, r31, ha16(L - L5pb)
	lwz 	r2, lo16(L - L5pb)(r2)
	addi 	r29, r2, 176
	stfd 	f0, 64(r30)
	lwz 	r2, 64(r30)
	lwz 	r3, 68(r30)
	mr 	r10, r3
	mr 	r9, r2
	stw 	r2, 64(r30)
	stw 	r3, 68(r30)
	lfd 	f13, 64(r30)
#	fmr 	f0, f13
	mr 	r3, r29
	addis 	r2, r31, ha16(LC1 - L5pb)
	la 	r4, lo16(LC1 - L5pb)(r2)
	mr 	r5, r9
	mr 	r6, r10
	fmr 	f1, f0
	bl 	fprintf
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r29, -12(r1)
	blr	
#	read_int
	.text
	.align 	2
	.globl	min_caml_read_int
min_caml_read_int:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -96(r1)
	mr 	r30, r1
	bcl 	20, 31, L6pb
L6pb:
	mflr 	r31
	addis 	r2, r31, ha16(LC0 - L6pb)
	la 	r3, lo16(LC0 - L6pb)(r2)
	addi 	r4, r30, 56
	bl 	scanf
	lwz 	r2, 56(r30)
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
scanf:	
	.indirect_symbol _scanf$LDBLStub
	mflr 	r0
	bcl 	20, 31, L6spb
L6spb:
	mflr 	r11
	addis 	r11, r11, ha16(scanf_lazy - L6spb)
	mtlr 	r0
	lwzu 	r12, lo16(scanf_lazy - L6spb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
scanf_lazy:	
	.indirect_symbol _scanf$LDBLStub
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
# 	read_float
	.text
	.align	2
	.globl	min_caml_read_float
min_caml_read_float:	
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -112(r1)
	mr 	r30, r1
	bcl 	20, 31, L7pb
L7pb:
	mflr	r31
	addis	r2, r31, ha16(LC1 - L7pb)
	la	r3, lo16(LC1 - L7pb)(r2)
	addi	r4, r30, 56
	bl	scanf
	lfd	f0, 56(r30)
	lwz	r1, 0(r1)
	lwz	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr	r0
	lmw	r30, -8(r1)
	blr
#	create_array
	.text
	.align	2
	.globl	min_caml_create_array
min_caml_create_array:	
	mr	r6, r2
	mr	r2, r4
create_array_loop:
	cmpwi	cr7, r6, 0
	bne	cr7, create_array_cont
	b	create_array_exit
create_array_exit:
	blr
create_array_cont:
	stw	r5, 0(r4)
	subi  	r6, r6, 1
	addi	r4, r4, 4
	b	create_array_loop
#	create_float_array
	.globl	min_caml_create_float_array
min_caml_create_float_array:
	mr	r5, r2
	mr	r2, r4
create_float_array_loop:
	cmpwi	cr7, r5, 0
	bne	cr7, create_float_array_cont
	blr
create_float_array_cont:
	stfd	f0, 0(r4)
	subi	r5, r5, 1
	addi	r4, r4, 8
	b	create_float_array_loop
	.globl	min_caml_abs_float
min_caml_abs_float:
	fabs	f0, f0
	blr
#	sqrt
	.text
	.align 	2
	.globl min_caml_sqrt
min_caml_sqrt:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -96(r1)
	mr 	r30, r1
	bcl 	20, 31, L8pb
L8pb:
	mflr	r31
	fmr	f1, f0
	bl 	sqrt
	fmr	f0, f1
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
sqrt:
	.indirect_symbol _sqrt
	mflr 	r0
	bcl 	20, 31, L8spb
L8spb:
	mflr 	r11
	addis 	r11, r11, ha16(sqrt_lazy - L8spb)
	mtlr 	r0
	lwzu 	r12, lo16(sqrt_lazy - L8spb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
sqrt_lazy:
	.indirect_symbol _sqrt
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
# 	floor
	.text
	.align 	2
	.globl 	min_caml_floor
min_caml_floor:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -80(r1)
	mr 	r30, r1
	fmr	f1, f0
	stfd 	f1, 56(r30)
	lfd 	f1, 56(r30)
	bl 	floor
	fmr 	f0, f1
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30,  -8(r1)
	blr	
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
floor:
	.indirect_symbol _floor
	mflr 	r0
	bcl 	20, 31, L9spb
L9spb:
	mflr 	r11
	addis 	r11, r11, ha16(floor_lazy - L9spb)
	mtlr 	r0
	lwzu 	r12, lo16(floor_lazy - L9spb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
floor_lazy:
	.indirect_symbol _floor
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
# 	int_of_float, truncate
	.text
	.align	2
	.globl	min_caml_int_of_float
min_caml_int_of_float:
	.globl	min_caml_truncate
min_caml_truncate:
	stmw 	r30, -8(r1)
	stwu 	r1, -64(r1)
	mr 	r30, r1
	stfd 	f0, 24(r30)
	lfd 	f1, 24(r30)
	fctiwz 	f1, f1
	stfd 	f1, 32(r30)
	lwz 	r31, 36(r30)
	mr 	r2, r31
	lwz 	r1, 0(r1)
	lmw 	r30, -8(r1)
	blr
#	float_of_int
	.globl	min_caml_float_of_int
min_caml_float_of_int:
	stmw 	r30, -8(r1)
	stw	r3, 8(r1)
	stw	r4, 12(r1)
	stwu 	r1, -48(r1)
	mr 	r30, r1
	mflr 	r0
	bcl 	20, 31, Lapb
Lapb:
	mflr 	r10
	mtlr 	r0
	stw 	r2, 72(r30)
	lwz 	r0, 72(r30)
	lis 	r2, 0x4330
	addis 	r9, r10, ha16(LC2 - Lapb)
	lfd 	f13, lo16(LC2 - Lapb)(r9)
	xoris 	r0, r0, 0x8000
	stw 	r0, 28(r30)
	stw 	r2, 24(r30)
	lfd 	f0, 24(r30)
	fsub 	f0, f0, f13
	lwz 	r1, 0(r1)
	lwz	r3, 8(r1)
	lwz	r4, 12(r1)
	lmw 	r30, -8(r1)
	blr
#	cos
	.text
	.align 	2
	.globl min_caml_cos
min_caml_cos:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -96(r1)
	mr 	r30, r1
	bcl 	20, 31, Lbpb
Lbpb:
	mflr 	r31
	fmr	f1, f0
	bl 	cos
	fmr 	f0, f1
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
cos:
	.indirect_symbol _cos
	mflr 	r0
	bcl 	20, 31, Lbspb
Lbspb:
	mflr 	r11
	addis 	r11, r11, ha16(cos_lazy - Lbspb)
	mtlr 	r0
	lwzu 	r12,lo16(cos_lazy - Lbspb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
cos_lazy:
	.indirect_symbol _cos
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
#	sin
	.text
	.align 	2
	.globl 	min_caml_sin
min_caml_sin:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -96(r1)
	mr 	r30, r1
	bcl 	20, 31, Lcpb
Lcpb:	
	mflr 	r31
	fmr	f1, f0
	bl 	sin
	fmr 	f0, f1
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
sin:	
	.indirect_symbol _sin
	mflr 	r0
	bcl 	20, 31, Lcspb
Lcspb:	
	mflr 	r11
	addis 	r11, r11, ha16(sin_lazy - Lcspb)
	mtlr 	r0
	lwzu 	r12, lo16(sin_lazy - Lcspb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
sin_lazy:	
	.indirect_symbol _sin
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
#	atan
	.text
	.align 	2
	.globl min_caml_atan
min_caml_atan:
	mflr 	r0
	stmw 	r30, -8(r1)
	stw 	r0, 8(r1)
	stw	r3, 12(r1)
	stw	r4, 16(r1)
	stwu 	r1, -96(r1)
	mr 	r30, r1
	bcl 	20, 31, Ldpb
Ldpb:
	mflr 	r31
	fmr	f1, f0
	bl 	atan
	fmr 	f0, f1
	lwz 	r1, 0(r1)
	lwz 	r0, 8(r1)
	lwz	r3, 12(r1)
	lwz	r4, 16(r1)
	mtlr 	r0
	lmw 	r30, -8(r1)
	blr
	.section __TEXT, __picsymbolstub1, symbol_stubs, pure_instructions, 32
	.align 	5
atan:
	.indirect_symbol _atan
	mflr 	r0
	bcl 	20, 31, Ldspb
Ldspb:
	mflr 	r11
	addis 	r11, r11, ha16(atan_lazy - Ldspb)
	mtlr 	r0
	lwzu 	r12, lo16(atan_lazy - Ldspb)(r11)
	mtctr 	r12
	bctr
	.lazy_symbol_pointer
atan_lazy:
	.indirect_symbol _atan
	.long	dyld_stub_binding_helper
	.subsections_via_symbols
	.const
	.align	8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x3ff00000
	.long	0x0
	
	
