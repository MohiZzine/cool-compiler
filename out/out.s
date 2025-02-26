	.text
	.file	"out.txt"
	.globl	Main_main                       # -- Begin function Main_main
	.p2align	4, 0x90
	.type	Main_main,@function
Main_main:                              # @Main_main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$12, %esi
	callq	IO_out_int@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	Main_main, .Lfunc_end0-Main_main
	.cfi_endproc
                                        # -- End function
	.globl	Main_new                        # -- Begin function Main_new
	.p2align	4, 0x90
	.type	Main_new,@function
Main_new:                               # @Main_new
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$8, %edi
	callq	malloc@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	Main_new, .Lfunc_end1-Main_new
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	Main_new@PLT
	movq	%rax, %rdi
	callq	Main_main@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.cfi_endproc
                                        # -- End function
	.globl	Object_abort                    # -- Begin function Object_abort
	.p2align	4, 0x90
	.type	Object_abort,@function
Object_abort:                           # @Object_abort
	.cfi_startproc
# %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$1, %edi
	callq	exit@PLT
	movq	%rbx, %rax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end3:
	.size	Object_abort, .Lfunc_end3-Object_abort
	.cfi_endproc
                                        # -- End function
	.globl	Object_type_name                # -- Begin function Object_type_name
	.p2align	4, 0x90
	.type	Object_type_name,@function
Object_type_name:                       # @Object_type_name
	.cfi_startproc
# %bb.0:
	movl	$.L.str0, %eax
	retq
.Lfunc_end4:
	.size	Object_type_name, .Lfunc_end4-Object_type_name
	.cfi_endproc
                                        # -- End function
	.globl	Object_copy                     # -- Begin function Object_copy
	.p2align	4, 0x90
	.type	Object_copy,@function
Object_copy:                            # @Object_copy
	.cfi_startproc
# %bb.0:
	movq	%rdi, %rax
	retq
.Lfunc_end5:
	.size	Object_copy, .Lfunc_end5-Object_copy
	.cfi_endproc
                                        # -- End function
	.globl	IO_out_string                   # -- Begin function IO_out_string
	.p2align	4, 0x90
	.type	IO_out_string,@function
IO_out_string:                          # @IO_out_string
	.cfi_startproc
# %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rsi, %rdi
	callq	puts@PLT
	movq	%rbx, %rax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end6:
	.size	IO_out_string, .Lfunc_end6-IO_out_string
	.cfi_endproc
                                        # -- End function
	.globl	IO_out_int                      # -- Begin function IO_out_int
	.p2align	4, 0x90
	.type	IO_out_int,@function
IO_out_int:                             # @IO_out_int
	.cfi_startproc
# %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$.L.fmt_int, %edi
	xorl	%eax, %eax
	callq	printf@PLT
	movq	%rbx, %rax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end7:
	.size	IO_out_int, .Lfunc_end7-IO_out_int
	.cfi_endproc
                                        # -- End function
	.globl	IO_in_string                    # -- Begin function IO_in_string
	.p2align	4, 0x90
	.type	IO_in_string,@function
IO_in_string:                           # @IO_in_string
	.cfi_startproc
# %bb.0:
	movl	$.L.empty_str, %eax
	retq
.Lfunc_end8:
	.size	IO_in_string, .Lfunc_end8-IO_in_string
	.cfi_endproc
                                        # -- End function
	.globl	IO_in_int                       # -- Begin function IO_in_int
	.p2align	4, 0x90
	.type	IO_in_int,@function
IO_in_int:                              # @IO_in_int
	.cfi_startproc
# %bb.0:
	movl	$42, %eax
	retq
.Lfunc_end9:
	.size	IO_in_int, .Lfunc_end9-IO_in_int
	.cfi_endproc
                                        # -- End function
	.globl	String_length                   # -- Begin function String_length
	.p2align	4, 0x90
	.type	String_length,@function
String_length:                          # @String_length
	.cfi_startproc
# %bb.0:
	movl	$999, %eax                      # imm = 0x3E7
	retq
.Lfunc_end10:
	.size	String_length, .Lfunc_end10-String_length
	.cfi_endproc
                                        # -- End function
	.globl	String_concat                   # -- Begin function String_concat
	.p2align	4, 0x90
	.type	String_concat,@function
String_concat:                          # @String_concat
	.cfi_startproc
# %bb.0:
	movq	%rdi, %rax
	retq
.Lfunc_end11:
	.size	String_concat, .Lfunc_end11-String_concat
	.cfi_endproc
                                        # -- End function
	.globl	String_substr                   # -- Begin function String_substr
	.p2align	4, 0x90
	.type	String_substr,@function
String_substr:                          # @String_substr
	.cfi_startproc
# %bb.0:
	movq	%rdi, %rax
	retq
.Lfunc_end12:
	.size	String_substr, .Lfunc_end12-String_substr
	.cfi_endproc
                                        # -- End function
	.type	.L.str0,@object                 # @.str0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str0:
	.asciz	"Object"
	.size	.L.str0, 7

	.type	.L.fmt_str,@object              # @.fmt_str
	.section	.rodata,"a",@progbits
.L.fmt_str:
	.asciz	"%s\n"
	.size	.L.fmt_str, 4

	.type	.L.fmt_int,@object              # @.fmt_int
.L.fmt_int:
	.asciz	"%d\n"
	.size	.L.fmt_int, 4

	.type	.L.empty_str,@object            # @.empty_str
.L.empty_str:
	.zero	1
	.size	.L.empty_str, 1

	.section	".note.GNU-stack","",@progbits
