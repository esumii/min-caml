#if defined(__CYGWIN__) || defined(__MACH__)
#define U(x) _##x
#else
#define U(x) x
#endif
#if defined(__MACH__)
#define ALIGNSTACK0 andl $0xfffffff0, %esp
#define ALIGNSTACK1 andl $0xfffffff0, %esp; pushl %eax; pushl %eax; pushl %eax
#define ALIGNSTACK2 andl $0xfffffff0, %esp; pushl %eax; pushl %eax
#define ALIGNSTACK3 andl $0xfffffff0, %esp; pushl %eax
#else
#define ALIGNSTACK0
#define ALIGNSTACK1
#define ALIGNSTACK2
#define ALIGNSTACK3
#endif
.text
.globl min_caml_print_newline
min_caml_print_newline:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK1
	pushl	$10
	call	U(putchar)
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_print_int
min_caml_print_int:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	pushl	%eax
	pushl	$format_int
	call	U(printf)
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_print_byte
min_caml_print_byte:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK1
	pushl	%eax
	call	U(putchar)
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_prerr_int
min_caml_prerr_int:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK3
	pushl	%eax
	pushl	$format_int
	pushl	U(min_caml_stderr)
	call	U(fprintf)
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_prerr_byte
min_caml_prerr_byte:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	pushl	U(min_caml_stderr)
	pushl	%eax
	call	U(fputc)
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_prerr_float
min_caml_prerr_float:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK0
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	pushl	$format_float
	pushl	U(min_caml_stderr)
	call	U(fprintf)
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_read_int
min_caml_read_int:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK3
	subl	$4, %esp
	leal	-4(%ebp), %eax
	pushl	%eax
	pushl	$format_int
	call	U(scanf)
	movl	-4(%ebp), %eax
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_read_float
min_caml_read_float:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK0
	subl	$8, %esp
	leal	-8(%ebp), %eax
	pushl	%eax
	pushl	$format_float
	call	U(scanf)
	movsd	-8(%ebp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_create_array
min_caml_create_array:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	pushl	%edx
	movl	%eax, %ecx
	movl	min_caml_hp, %eax
	movl	%ecx, %edx
	shll	$2, %edx
	addl	%edx, min_caml_hp
create_array_loop:
	cmpl	$0, %ecx
	jne	create_array_cont
create_array_exit:
	popl	%edx
	popl	%ecx
	movl	%ebp, %esp
	popl	%ebp
	ret
create_array_cont:
	decl	%ecx
	movl	%ebx, (%eax,%ecx,4)
	jmp	create_array_loop
.globl min_caml_create_float_array
min_caml_create_float_array:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	pushl	%edx
	movl	%eax, %ecx
	movl	min_caml_hp, %eax
	movl	%ecx, %edx
	shll	$3, %edx
	addl	%edx, min_caml_hp
create_float_array_loop:
	cmpl	$0, %ecx
	jne	create_float_array_cont
create_float_array_exit:
	popl	%edx
	popl	%ecx
	movl	%ebp, %esp
	popl	%ebp
	ret
create_float_array_cont:
	decl	%ecx
	movsd	%xmm0, (%eax,%ecx,8)
	jmp	create_float_array_loop
.globl min_caml_abs_float
min_caml_abs_float:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(fabs)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_sqrt
min_caml_sqrt:
	sqrtsd	%xmm0, %xmm0
	ret
.globl min_caml_floor
min_caml_floor:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(floor)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_int_of_float
min_caml_int_of_float:
.globl min_caml_truncate
min_caml_truncate:
	cvttsd2si	%xmm0, %eax
	ret
.globl min_caml_float_of_int
min_caml_float_of_int:
	cvtsi2sd	%eax, %xmm0
	ret
.globl min_caml_cos
min_caml_cos:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(cos)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_sin
min_caml_sin:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(sin)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.globl min_caml_atan
min_caml_atan:
	pushl	%ebp
	movl	%esp, %ebp
	ALIGNSTACK2
	subl	$8, %esp
	movsd	%xmm0, (%esp)
	call	U(atan)
	fstpl	(%esp)
	movsd	(%esp), %xmm0
	movl	%ebp, %esp
	popl	%ebp
	ret
.data
format_int:
	.asciz	"%d"
format_float:
	.asciz	"%lf"
.balign 8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x0
	.long	0x3ff00000
.balign 16
.globl min_caml_fnegd
min_caml_fnegd:
	.long	0
	.long	0x80000000
	.long	0
	.long	0
.globl min_caml_hp
min_caml_hp:	
	.long	0x0
