	.file	"POSIX.extern.bc"
	.section	.debug_frame,"",@progbits
.Lsection_debug_frame:
	.section	.debug_info,"",@progbits
.Lsection_info:
	.section	.debug_abbrev,"",@progbits
.Lsection_abbrev:
	.section	.debug_aranges,"",@progbits
.Lsection_aranges:
	.section	.debug_macinfo,"",@progbits
.Lsection_macinfo:
	.section	.debug_line,"",@progbits
.Lsection_line:
	.section	.debug_loc,"",@progbits
.Lsection_loc:
	.section	.debug_pubnames,"",@progbits
.Lsection_pubnames:
	.section	.debug_str,"",@progbits
.Lsection_str:
	.section	.debug_ranges,"",@progbits
.Lsection_ranges:
	.text
.Ltext_begin:
	.data
.Ldata_begin:


	.text
	.align	16
	.globl	mkHost
	.type	mkHost,@function
mkHost:                                                     # @mkHost
.Lfunc_begin1:
.Llabel5:
.LBB1_0:                                                    # %entry
	pushl	%esi
.Llabel3:
	subl	$32, %esp
.Llabel4:
	leal	40(%esp), %esi
	.align	16
.LBB1_1:                                                    # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel6:
	movl	hp, %eax                                    # SrcLine 121
	movl	%eax, 16(%esp)                              # SrcLine 121
	movl	lim, %eax                                   # SrcLine 121
	movl	%eax, 24(%esp)                              # SrcLine 121
	movl	16(%esp), %ecx                              # SrcLine 121
	addl	$8, %ecx                                    # SrcLine 121
	movl	%ecx, 20(%esp)                              # SrcLine 121
	movl	16(%esp), %edx                              # SrcLine 121
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 121
	cmpl	%edx, %eax                                  # SrcLine 121
	jne	.LBB1_1                                     # SrcLine 121
.LBB1_2:                                                    # %bb4
	movl	24(%esp), %eax                              # SrcLine 121
	cmpl	%eax, 20(%esp)                              # SrcLine 121
	jb	.LBB1_6
.LBB1_3:                                                    # %bb5
	movl	24(%esp), %eax                              # SrcLine 121
	cmpl	%eax, 16(%esp)                              # SrcLine 121
	jb	.LBB1_8
.LBB1_4:                                                    # %bb7
	movl	$0, 8(%esp)                                 # SrcLine 121
.LBB1_5:                                                    # %bb8
	movl	8(%esp), %eax                               # SrcLine 121
	movl	%eax, 4(%esp)                               # SrcLine 121
	movl	$2, (%esp)                                  # SrcLine 121
	call	force                                       # SrcLine 121
	movl	%eax, 16(%esp)                              # SrcLine 121
.LBB1_6:                                                    # %bb9
.Llabel7:
	movl	16(%esp), %eax                              # SrcLine 122
	movl	$__GC___Host_POSIX, (%eax)                  # SrcLine 122
.Llabel8:
	movl	4(%esi), %eax                               # SrcLine 123
	movl	%eax, (%esp)                                # SrcLine 123
	call	inet_ntoa                                   # SrcLine 123
	movl	%eax, (%esp)                                # SrcLine 123
	call	getStr                                      # SrcLine 123
	movl	16(%esp), %ecx                              # SrcLine 123
	movl	%eax, 4(%ecx)                               # SrcLine 123
.Llabel9:
	movl	16(%esp), %eax                              # SrcLine 124
	movl	%eax, 12(%esp)                              # SrcLine 124
	movl	%eax, 28(%esp)                              # SrcLine 124
.LBB1_7:                                                    # %return
	movl	28(%esp), %eax
.Llabel2:
	addl	$32, %esp                                   # SrcLine 124
	popl	%esi                                        # SrcLine 124
	ret                                                 # SrcLine 124
.LBB1_8:                                                    # %bb6
.Llabel10:
	movl	16(%esp), %eax                              # SrcLine 121
	movl	%eax, 8(%esp)                               # SrcLine 121
	jmp	.LBB1_5
	.size	mkHost, .-mkHost
.Lfunc_end1:


	.align	16
	.globl	mkPort
	.type	mkPort,@function
mkPort:                                                     # @mkPort
.Lfunc_begin2:
.Llabel15:
.LBB2_0:                                                    # %entry
	pushl	%esi
.Llabel13:
	subl	$32, %esp
.Llabel14:
	leal	40(%esp), %esi
	.align	16
.LBB2_1:                                                    # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel16:
	movl	hp, %eax                                    # SrcLine 128
	movl	%eax, 16(%esp)                              # SrcLine 128
	movl	lim, %eax                                   # SrcLine 128
	movl	%eax, 24(%esp)                              # SrcLine 128
	movl	16(%esp), %ecx                              # SrcLine 128
	addl	$8, %ecx                                    # SrcLine 128
	movl	%ecx, 20(%esp)                              # SrcLine 128
	movl	16(%esp), %edx                              # SrcLine 128
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 128
	cmpl	%edx, %eax                                  # SrcLine 128
	jne	.LBB2_1                                     # SrcLine 128
.LBB2_2:                                                    # %bb4
	movl	24(%esp), %eax                              # SrcLine 128
	cmpl	%eax, 20(%esp)                              # SrcLine 128
	jb	.LBB2_6
.LBB2_3:                                                    # %bb5
	movl	24(%esp), %eax                              # SrcLine 128
	cmpl	%eax, 16(%esp)                              # SrcLine 128
	jb	.LBB2_8
.LBB2_4:                                                    # %bb7
	movl	$0, 8(%esp)                                 # SrcLine 128
.LBB2_5:                                                    # %bb8
	movl	8(%esp), %eax                               # SrcLine 128
	movl	%eax, 4(%esp)                               # SrcLine 128
	movl	$2, (%esp)                                  # SrcLine 128
	call	force                                       # SrcLine 128
	movl	%eax, 16(%esp)                              # SrcLine 128
.LBB2_6:                                                    # %bb9
.Llabel17:
	movl	16(%esp), %eax                              # SrcLine 129
	movl	$__GC___Port_POSIX, (%eax)                  # SrcLine 129
.Llabel18:
	movzwl	2(%esi), %eax                               # SrcLine 130
	movl	%eax, (%esp)                                # SrcLine 130
	call	ntohs                                       # SrcLine 130
	movzwl	%ax, %eax                                   # SrcLine 130
	movl	16(%esp), %ecx                              # SrcLine 130
	movl	%eax, 4(%ecx)                               # SrcLine 130
.Llabel19:
	movl	16(%esp), %eax                              # SrcLine 131
	movl	%eax, 12(%esp)                              # SrcLine 131
	movl	%eax, 28(%esp)                              # SrcLine 131
.LBB2_7:                                                    # %return
	movl	28(%esp), %eax
.Llabel12:
	addl	$32, %esp                                   # SrcLine 131
	popl	%esi                                        # SrcLine 131
	ret                                                 # SrcLine 131
.LBB2_8:                                                    # %bb6
.Llabel20:
	movl	16(%esp), %eax                              # SrcLine 128
	movl	%eax, 8(%esp)                               # SrcLine 128
	jmp	.LBB2_5
	.size	mkPort, .-mkPort
.Lfunc_end2:


	.align	16
	.globl	close_fun
	.type	close_fun,@function
close_fun:                                                  # @close_fun
.Lfunc_begin3:
.Llabel24:
.LBB3_0:                                                    # %entry
	subl	$28, %esp
.Llabel23:
	movl	32(%esp), %eax
	movl	%eax, 24(%esp)
	movl	36(%esp), %eax
	movl	%eax, 20(%esp)
.Llabel25:
	movl	$envmut, (%esp)                             # SrcLine 164
	call	pthread_mutex_lock                          # SrcLine 164
.Llabel26:
	movl	24(%esp), %eax                              # SrcLine 165
	movl	8(%eax), %eax                               # SrcLine 165
	movl	%eax, 12(%esp)                              # SrcLine 165
.Llabel27:
	movl	%eax, (%esp)                                # SrcLine 166
	call	close                                       # SrcLine 166
.Llabel28:
	movl	12(%esp), %eax                              # SrcLine 167
	movl	$0, rdTable(,%eax,4)                        # SrcLine 167
	movl	12(%esp), %eax                              # SrcLine 167
	movl	%eax, %ecx                                  # SrcLine 167
	andl	$31, %ecx                                   # SrcLine 167
	andl	$4294967264, %eax                           # SrcLine 167
	shrl	$3, %eax                                    # SrcLine 167
	#APP
	btrl %ecx,readUsed(%eax)
	#NO_APP
.Llabel29:
	movl	12(%esp), %eax                              # SrcLine 168
	movl	$0, wrTable(,%eax,4)                        # SrcLine 168
	movl	12(%esp), %eax                              # SrcLine 168
	movl	%eax, %ecx                                  # SrcLine 168
	andl	$31, %ecx                                   # SrcLine 168
	andl	$4294967264, %eax                           # SrcLine 168
	shrl	$3, %eax                                    # SrcLine 168
	#APP
	btrl %ecx,writeUsed(%eax)
	#NO_APP
.Llabel30:
	movl	12(%esp), %eax                              # SrcLine 169
	movl	$0, sockTable(,%eax,4)                      # SrcLine 169
.Llabel31:
	cmpl	$0, eventThread                             # SrcLine 170
	je	.LBB3_2
.LBB3_1:                                                    # %bb
	movl	eventThread, %eax                           # SrcLine 170
	movl	12(%eax), %eax                              # SrcLine 170
	movl	%eax, (%esp)                                # SrcLine 170
	movl	$10, 4(%esp)                                # SrcLine 170
	call	pthread_kill                                # SrcLine 170
.LBB3_2:                                                    # %bb1
.Llabel32:
	movl	$envmut, (%esp)                             # SrcLine 171
	call	pthread_mutex_unlock                        # SrcLine 171
.Llabel33:
	movl	$0, 8(%esp)                                 # SrcLine 172
	movl	$0, 16(%esp)                                # SrcLine 172
.LBB3_3:                                                    # %return
	movsbl	16(%esp), %eax                              # SrcLine 172
.Llabel22:
	addl	$28, %esp                                   # SrcLine 172
	ret                                                 # SrcLine 172
	.size	close_fun, .-close_fun
.Lfunc_end3:


	.align	16
	.globl	new_Closable
	.type	new_Closable,@function
new_Closable:                                               # @new_Closable
.Lfunc_begin4:
.Llabel37:
.LBB4_0:                                                    # %entry
	subl	$36, %esp
.Llabel36:
	movl	40(%esp), %eax
	movl	%eax, 32(%esp)
	.align	16
.LBB4_1:                                                    # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel38:
	movl	hp, %eax                                    # SrcLine 177
	movl	%eax, 16(%esp)                              # SrcLine 177
	movl	lim, %eax                                   # SrcLine 177
	movl	%eax, 24(%esp)                              # SrcLine 177
	movl	16(%esp), %ecx                              # SrcLine 177
	addl	$12, %ecx                                   # SrcLine 177
	movl	%ecx, 20(%esp)                              # SrcLine 177
	movl	16(%esp), %edx                              # SrcLine 177
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 177
	cmpl	%edx, %eax                                  # SrcLine 177
	jne	.LBB4_1                                     # SrcLine 177
.LBB4_2:                                                    # %bb4
	movl	24(%esp), %eax                              # SrcLine 177
	cmpl	%eax, 20(%esp)                              # SrcLine 177
	jb	.LBB4_6
.LBB4_3:                                                    # %bb5
	movl	24(%esp), %eax                              # SrcLine 177
	cmpl	%eax, 16(%esp)                              # SrcLine 177
	jb	.LBB4_8
.LBB4_4:                                                    # %bb7
	movl	$0, 8(%esp)                                 # SrcLine 177
.LBB4_5:                                                    # %bb8
	movl	8(%esp), %eax                               # SrcLine 177
	movl	%eax, 4(%esp)                               # SrcLine 177
	movl	$3, (%esp)                                  # SrcLine 177
	call	force                                       # SrcLine 177
	movl	%eax, 16(%esp)                              # SrcLine 177
.LBB4_6:                                                    # %bb9
.Llabel39:
	movl	16(%esp), %eax                              # SrcLine 178
	movl	$__GC__DescClosable, (%eax)                 # SrcLine 178
.Llabel40:
	movl	16(%esp), %eax                              # SrcLine 179
	movl	$close_fun, 4(%eax)                         # SrcLine 179
.Llabel41:
	movl	32(%esp), %eax                              # SrcLine 180
	movl	16(%esp), %ecx                              # SrcLine 180
	movl	%eax, 8(%ecx)                               # SrcLine 180
.Llabel42:
	movl	16(%esp), %eax                              # SrcLine 181
	movl	%eax, 12(%esp)                              # SrcLine 181
	movl	%eax, 28(%esp)                              # SrcLine 181
.LBB4_7:                                                    # %return
	movl	28(%esp), %eax
.Llabel35:
	addl	$36, %esp                                   # SrcLine 181
	ret                                                 # SrcLine 181
.LBB4_8:                                                    # %bb6
.Llabel43:
	movl	16(%esp), %eax                              # SrcLine 177
	movl	%eax, 8(%esp)                               # SrcLine 177
	jmp	.LBB4_5
	.size	new_Closable, .-new_Closable
.Lfunc_end4:


	.align	16
	.globl	seek_fun
	.type	seek_fun,@function
seek_fun:                                                   # @seek_fun
.Lfunc_begin5:
.Llabel47:
.LBB5_0:                                                    # %entry
	subl	$44, %esp
.Llabel46:
	movl	48(%esp), %eax
	movl	%eax, 40(%esp)
	movl	52(%esp), %eax
	movl	%eax, 36(%esp)
	movl	56(%esp), %eax
	movl	%eax, 32(%esp)
.Llabel48:
	movl	$envmut, (%esp)                             # SrcLine 187
	call	pthread_mutex_lock                          # SrcLine 187
.Llabel49:
	cmpl	$0, 36(%esp)                                # SrcLine 189
	jns	.LBB5_4
.LBB5_1:                                                    # %bb1
.Llabel50:
	movl	$2, 24(%esp)                                # SrcLine 192
.Llabel51:
	incl	36(%esp)                                    # SrcLine 193
.LBB5_2:                                                    # %bb2
.Llabel52:
	movl	40(%esp), %eax                              # SrcLine 195
	movl	4(%eax), %eax                               # SrcLine 195
	movl	8(%eax), %eax                               # SrcLine 195
	movl	36(%esp), %ecx                              # SrcLine 195
	movl	24(%esp), %edx                              # SrcLine 195
	movl	%edx, 8(%esp)                               # SrcLine 195
	movl	%ecx, 4(%esp)                               # SrcLine 195
	movl	%eax, (%esp)                                # SrcLine 195
	call	lseek                                       # SrcLine 195
	movl	%eax, 20(%esp)                              # SrcLine 195
.Llabel53:
	movl	$envmut, (%esp)                             # SrcLine 196
	call	pthread_mutex_unlock                        # SrcLine 196
.Llabel54:
	movl	20(%esp), %eax                              # SrcLine 197
	movl	%eax, 16(%esp)                              # SrcLine 197
	movl	%eax, 28(%esp)                              # SrcLine 197
.LBB5_3:                                                    # %return
	movl	28(%esp), %eax
.Llabel45:
	addl	$44, %esp                                   # SrcLine 197
	ret                                                 # SrcLine 197
.LBB5_4:                                                    # %bb
.Llabel55:
	movl	$0, 24(%esp)                                # SrcLine 190
	jmp	.LBB5_2
	.size	seek_fun, .-seek_fun
.Lfunc_end5:


	.align	16
	.globl	new_File
	.type	new_File,@function
new_File:                                                   # @new_File
.Lfunc_begin6:
.Llabel59:
.LBB6_0:                                                    # %entry
	subl	$36, %esp
.Llabel58:
	movl	40(%esp), %eax
	movl	%eax, 32(%esp)
	.align	16
.LBB6_1:                                                    # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel60:
	movl	hp, %eax                                    # SrcLine 201
	movl	%eax, 16(%esp)                              # SrcLine 201
	movl	lim, %eax                                   # SrcLine 201
	movl	%eax, 24(%esp)                              # SrcLine 201
	movl	16(%esp), %ecx                              # SrcLine 201
	addl	$12, %ecx                                   # SrcLine 201
	movl	%ecx, 20(%esp)                              # SrcLine 201
	movl	16(%esp), %edx                              # SrcLine 201
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 201
	cmpl	%edx, %eax                                  # SrcLine 201
	jne	.LBB6_1                                     # SrcLine 201
.LBB6_2:                                                    # %bb4
	movl	24(%esp), %eax                              # SrcLine 201
	cmpl	%eax, 20(%esp)                              # SrcLine 201
	jb	.LBB6_6
.LBB6_3:                                                    # %bb5
	movl	24(%esp), %eax                              # SrcLine 201
	cmpl	%eax, 16(%esp)                              # SrcLine 201
	jb	.LBB6_8
.LBB6_4:                                                    # %bb7
	movl	$0, 8(%esp)                                 # SrcLine 201
.LBB6_5:                                                    # %bb8
	movl	8(%esp), %eax                               # SrcLine 201
	movl	%eax, 4(%esp)                               # SrcLine 201
	movl	$3, (%esp)                                  # SrcLine 201
	call	force                                       # SrcLine 201
	movl	%eax, 16(%esp)                              # SrcLine 201
.LBB6_6:                                                    # %bb9
.Llabel61:
	movl	16(%esp), %eax                              # SrcLine 202
	movl	$__GC__File_POSIX, (%eax)                   # SrcLine 202
.Llabel62:
	movl	32(%esp), %eax                              # SrcLine 203
	movl	%eax, (%esp)                                # SrcLine 203
	call	new_Closable                                # SrcLine 203
	movl	16(%esp), %ecx                              # SrcLine 203
	movl	%eax, 4(%ecx)                               # SrcLine 203
.Llabel63:
	movl	16(%esp), %eax                              # SrcLine 204
	movl	$seek_fun, 8(%eax)                          # SrcLine 204
.Llabel64:
	movl	16(%esp), %eax                              # SrcLine 205
	movl	%eax, 12(%esp)                              # SrcLine 205
	movl	%eax, 28(%esp)                              # SrcLine 205
.LBB6_7:                                                    # %return
	movl	28(%esp), %eax
.Llabel57:
	addl	$36, %esp                                   # SrcLine 205
	ret                                                 # SrcLine 205
.LBB6_8:                                                    # %bb6
.Llabel65:
	movl	16(%esp), %eax                              # SrcLine 201
	movl	%eax, 8(%esp)                               # SrcLine 201
	jmp	.LBB6_5
	.size	new_File, .-new_File
.Lfunc_end6:


	.align	16
	.globl	read_descr
	.type	read_descr,@function
read_descr:                                                 # @read_descr
.Lfunc_begin7:
.Llabel70:
.LBB7_0:                                                    # %entry
	pushl	%esi
.Llabel68:
	subl	$1088, %esp
.Llabel69:
	movl	1096(%esp), %eax
	movl	%eax, 1084(%esp)
.Llabel71:
	movl	$0, 1048(%esp)                              # SrcLine 212
.Llabel72:
	movl	$0, 1052(%esp)                              # SrcLine 213
.Llabel73:
	movl	$0, 1056(%esp)                              # SrcLine 214
.Llabel74:
	movl	$0, 1060(%esp)                              # SrcLine 215
	leal	24(%esp), %esi
	.align	16
.LBB7_1:                                                    # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            #   Child Loop BB7_14 Depth 2
                                                            #     Child Loop BB7_6 Depth 3
.Llabel75:
	movl	$0, 1048(%esp)                              # SrcLine 218
.Llabel76:
	movl	$0, 1052(%esp)                              # SrcLine 219
.Llabel77:
	movl	1084(%esp), %eax                            # SrcLine 220
	movl	%esi, 4(%esp)                               # SrcLine 220
	movl	%eax, (%esp)                                # SrcLine 220
	movl	$1023, 8(%esp)                              # SrcLine 220
	call	read                                        # SrcLine 220
	movl	%eax, 1064(%esp)                            # SrcLine 220
	testl	%eax, %eax                                  # SrcLine 220
	jg	.LBB7_14
.LBB7_2:                                                    # %bb2
.Llabel78:
	cmpl	$0, 1060(%esp)                              # SrcLine 222
	je	.LBB7_4
.LBB7_3:                                                    # %bb3
	movl	1060(%esp), %esi                            # SrcLine 222
	movl	$0, 8(%esi)                                 # SrcLine 222
.LBB7_4:                                                    # %bb4
.Llabel79:
	movl	1056(%esp), %esi                            # SrcLine 223
	movl	%esi, 16(%esp)                              # SrcLine 223
	movl	%esi, 1080(%esp)                            # SrcLine 223
.LBB7_5:                                                    # %return
	movl	1080(%esp), %eax
.Llabel80:
.Llabel67:
	addl	$1088, %esp                                 # SrcLine 237
	popl	%esi                                        # SrcLine 237
	ret                                                 # SrcLine 237
	.align	16
.LBB7_6:                                                    # %bb6
                                                            # Loop Depth 3
                                                            # Loop Header
                                                            # Inner Loop
                                                            #   Inside Loop BB7_14 Depth 2
                                                            # Inside Loop BB7_1 Depth 1
.Llabel81:
	movl	hp, %eax                                    # SrcLine 226
	movl	%eax, 1068(%esp)                            # SrcLine 226
	movl	lim, %eax                                   # SrcLine 226
	movl	%eax, 1076(%esp)                            # SrcLine 226
	movl	1068(%esp), %ecx                            # SrcLine 226
	addl	$12, %ecx                                   # SrcLine 226
	movl	%ecx, 1072(%esp)                            # SrcLine 226
	movl	1068(%esp), %edx                            # SrcLine 226
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 226
	cmpl	%edx, %eax                                  # SrcLine 226
	jne	.LBB7_6                                     # SrcLine 226
.LBB7_7:                                                    # %bb10
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
	movl	1076(%esp), %eax                            # SrcLine 226
	cmpl	%eax, 1072(%esp)                            # SrcLine 226
	jb	.LBB7_11
.LBB7_8:                                                    # %bb11
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
	movl	1076(%esp), %eax                            # SrcLine 226
	cmpl	%eax, 1068(%esp)                            # SrcLine 226
	jb	.LBB7_18
.LBB7_9:                                                    # %bb13
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
	movl	$0, 20(%esp)                                # SrcLine 226
.LBB7_10:                                                   # %bb14
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
	movl	20(%esp), %eax                              # SrcLine 226
	movl	%eax, 4(%esp)                               # SrcLine 226
	movl	$3, (%esp)                                  # SrcLine 226
	call	force                                       # SrcLine 226
	movl	%eax, 1068(%esp)                            # SrcLine 226
.LBB7_11:                                                   # %bb15
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
.Llabel82:
	cmpl	$0, 1052(%esp)                              # SrcLine 227
	jne	.LBB7_13
.LBB7_12:                                                   # %bb16
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
	movl	1068(%esp), %eax                            # SrcLine 227
	movl	%eax, 1052(%esp)                            # SrcLine 227
.LBB7_13:                                                   # %bb17
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
.Llabel83:
	movl	1068(%esp), %eax                            # SrcLine 228
	movl	$__GC__CONS+20, (%eax)                      # SrcLine 228
.Llabel84:
	movl	1064(%esp), %eax                            # SrcLine 229
	leal	-1(%eax), %ecx                              # SrcLine 229
	movl	%ecx, 1064(%esp)                            # SrcLine 229
	movsbl	23(%esp,%eax), %eax                         # SrcLine 229
	movl	1068(%esp), %ecx                            # SrcLine 229
	movl	%eax, 4(%ecx)                               # SrcLine 229
.Llabel85:
	movl	1048(%esp), %eax                            # SrcLine 230
	movl	1068(%esp), %ecx                            # SrcLine 230
	movl	%eax, 8(%ecx)                               # SrcLine 230
.Llabel86:
	movl	1068(%esp), %eax                            # SrcLine 231
	movl	%eax, 1048(%esp)                            # SrcLine 231
.LBB7_14:                                                   # %bb18
                                                            # Loop Depth 2
                                                            # Loop Header
                                                            #     Child Loop BB7_6 Depth 3
                                                            # Inside Loop BB7_1 Depth 1
.Llabel87:
	cmpl	$0, 1064(%esp)                              # SrcLine 225
	jne	.LBB7_6
.LBB7_15:                                                   # %bb19
                                                            # Loop Depth 1
                                                            # Loop Header is BB7_1
.Llabel88:
	cmpl	$0, 1056(%esp)                              # SrcLine 233
	je	.LBB7_19
.LBB7_16:                                                   # %bb21
                                                            # Loop Depth 1
                                                            # Loop Header is BB7_1
.Llabel89:
	movl	1048(%esp), %eax                            # SrcLine 236
	movl	1060(%esp), %ecx                            # SrcLine 236
	movl	%eax, 8(%ecx)                               # SrcLine 236
.LBB7_17:                                                   # %bb22
                                                            # Loop Depth 1
                                                            # Loop Header is BB7_1
.Llabel90:
	movl	1052(%esp), %eax                            # SrcLine 237
	movl	%eax, 1060(%esp)                            # SrcLine 237
	jmp	.LBB7_1
.LBB7_18:                                                   # %bb12
                                                            # Loop Depth 2
                                                            # Loop Header is BB7_14
                                                            # Inside Loop BB7_1 Depth 1
.Llabel91:
	movl	1068(%esp), %eax                            # SrcLine 226
	movl	%eax, 20(%esp)                              # SrcLine 226
	jmp	.LBB7_10
.LBB7_19:                                                   # %bb20
                                                            # Loop Depth 1
                                                            # Loop Header is BB7_1
.Llabel92:
	movl	1048(%esp), %eax                            # SrcLine 234
	movl	%eax, 1056(%esp)                            # SrcLine 234
	jmp	.LBB7_17
	.size	read_descr, .-read_descr
.Lfunc_end7:


	.align	16
	.globl	read_fun
	.type	read_fun,@function
read_fun:                                                   # @read_fun
.Lfunc_begin8:
.Llabel96:
.LBB8_0:                                                    # %entry
	subl	$20, %esp
.Llabel95:
	movl	24(%esp), %eax
	movl	%eax, 16(%esp)
	movl	28(%esp), %eax
	movl	%eax, 12(%esp)
.Llabel97:
	movl	16(%esp), %eax                              # SrcLine 243
	movl	4(%eax), %eax                               # SrcLine 243
	movl	4(%eax), %eax                               # SrcLine 243
	movl	8(%eax), %eax                               # SrcLine 243
	movl	%eax, (%esp)                                # SrcLine 243
	call	read_descr                                  # SrcLine 243
	movl	%eax, 4(%esp)                               # SrcLine 243
	movl	%eax, 8(%esp)                               # SrcLine 243
.LBB8_1:                                                    # %return
	movl	8(%esp), %eax
.Llabel94:
	addl	$20, %esp                                   # SrcLine 243
	ret                                                 # SrcLine 243
	.size	read_fun, .-read_fun
.Lfunc_end8:


	.align	16
	.globl	installR_fun
	.type	installR_fun,@function
installR_fun:                                               # @installR_fun
.Lfunc_begin9:
.Llabel101:
.LBB9_0:                                                    # %entry
	subl	$44, %esp
.Llabel100:
	movl	48(%esp), %eax
	movl	%eax, 40(%esp)
	movl	52(%esp), %eax
	movl	%eax, 36(%esp)
	movl	56(%esp), %eax
	movl	%eax, 32(%esp)
.Llabel102:
	movl	$envmut, (%esp)                             # SrcLine 247
	call	pthread_mutex_lock                          # SrcLine 247
.Llabel103:
	movl	40(%esp), %eax                              # SrcLine 248
	movl	4(%eax), %eax                               # SrcLine 248
	movl	4(%eax), %eax                               # SrcLine 248
	movl	8(%eax), %eax                               # SrcLine 248
	movl	%eax, 16(%esp)                              # SrcLine 248
.Llabel104:
	movl	%eax, %ecx                                  # SrcLine 249
	andl	$31, %ecx                                   # SrcLine 249
	andl	$4294967264, %eax                           # SrcLine 249
	shrl	$3, %eax                                    # SrcLine 249
	#APP
	btl %ecx,readUsed(%eax) ; setcb %al
	#NO_APP
	movb	%al, 27(%esp)                               # SrcLine 249
	movb	%al, 11(%esp)                               # SrcLine 249
	movsbl	11(%esp), %eax                              # SrcLine 249
	movl	%eax, 20(%esp)                              # SrcLine 249
.Llabel105:
	movl	36(%esp), %eax                              # SrcLine 250
	movl	16(%esp), %ecx                              # SrcLine 250
	movl	%eax, rdTable(,%ecx,4)                      # SrcLine 250
	movl	16(%esp), %eax                              # SrcLine 250
	movl	%eax, %ecx                                  # SrcLine 250
	andl	$31, %ecx                                   # SrcLine 250
	andl	$4294967264, %eax                           # SrcLine 250
	shrl	$3, %eax                                    # SrcLine 250
	#APP
	btsl %ecx,readUsed(%eax)
	#NO_APP
.Llabel106:
	movl	16(%esp), %eax                              # SrcLine 251
	movl	maxDesc, %ecx                               # SrcLine 251
	cmpl	%eax, %ecx                                  # SrcLine 251
	cmovge	%ecx, %eax                                  # SrcLine 251
	movl	%eax, maxDesc                               # SrcLine 251
.Llabel107:
	cmpl	$0, eventThread                             # SrcLine 252
	je	.LBB9_5
.LBB9_1:                                                    # %bb2
.Llabel108:
	cmpl	$0, 20(%esp)                                # SrcLine 253
	jne	.LBB9_3
.LBB9_2:                                                    # %bb3
	movl	eventThread, %eax                           # SrcLine 253
	movl	12(%eax), %eax                              # SrcLine 253
	movl	%eax, (%esp)                                # SrcLine 253
	movl	$10, 4(%esp)                                # SrcLine 253
	call	pthread_kill                                # SrcLine 253
.LBB9_3:                                                    # %bb4
.Llabel109:
	movl	$envmut, (%esp)                             # SrcLine 254
	call	pthread_mutex_unlock                        # SrcLine 254
.Llabel110:
	movl	$0, 12(%esp)                                # SrcLine 255
	movl	$0, 28(%esp)                                # SrcLine 255
.LBB9_4:                                                    # %return
	movsbl	28(%esp), %eax                              # SrcLine 255
.Llabel99:
	addl	$44, %esp                                   # SrcLine 255
	ret                                                 # SrcLine 255
.LBB9_5:                                                    # %bb
.Llabel111:
	call	startLoop                                   # SrcLine 252
	jmp	.LBB9_3
	.size	installR_fun, .-installR_fun
.Lfunc_end9:


	.align	16
	.globl	new_RFile
	.type	new_RFile,@function
new_RFile:                                                  # @new_RFile
.Lfunc_begin10:
.Llabel115:
.LBB10_0:                                                   # %entry
	subl	$36, %esp
.Llabel114:
	movl	40(%esp), %eax
	movl	%eax, 32(%esp)
	.align	16
.LBB10_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel116:
	movl	hp, %eax                                    # SrcLine 259
	movl	%eax, 16(%esp)                              # SrcLine 259
	movl	lim, %eax                                   # SrcLine 259
	movl	%eax, 24(%esp)                              # SrcLine 259
	movl	16(%esp), %ecx                              # SrcLine 259
	addl	$64, %ecx                                   # SrcLine 259
	movl	%ecx, 20(%esp)                              # SrcLine 259
	movl	16(%esp), %edx                              # SrcLine 259
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 259
	cmpl	%edx, %eax                                  # SrcLine 259
	jne	.LBB10_1                                    # SrcLine 259
.LBB10_2:                                                   # %bb4
	movl	24(%esp), %eax                              # SrcLine 259
	cmpl	%eax, 20(%esp)                              # SrcLine 259
	jb	.LBB10_6
.LBB10_3:                                                   # %bb5
	movl	24(%esp), %eax                              # SrcLine 259
	cmpl	%eax, 16(%esp)                              # SrcLine 259
	jb	.LBB10_8
.LBB10_4:                                                   # %bb7
	movl	$0, 8(%esp)                                 # SrcLine 259
.LBB10_5:                                                   # %bb8
	movl	8(%esp), %eax                               # SrcLine 259
	movl	%eax, 4(%esp)                               # SrcLine 259
	movl	$16, (%esp)                                 # SrcLine 259
	call	force                                       # SrcLine 259
	movl	%eax, 16(%esp)                              # SrcLine 259
.LBB10_6:                                                   # %bb9
.Llabel117:
	movl	16(%esp), %eax                              # SrcLine 260
	movl	$__GC__RFile_POSIX, (%eax)                  # SrcLine 260
.Llabel118:
	movl	32(%esp), %eax                              # SrcLine 261
	movl	%eax, (%esp)                                # SrcLine 261
	call	new_File                                    # SrcLine 261
	movl	16(%esp), %ecx                              # SrcLine 261
	movl	%eax, 4(%ecx)                               # SrcLine 261
.Llabel119:
	movl	16(%esp), %eax                              # SrcLine 262
	movl	$read_fun, 8(%eax)                          # SrcLine 262
.Llabel120:
	movl	16(%esp), %eax                              # SrcLine 263
	movl	$installR_fun, 12(%eax)                     # SrcLine 263
.Llabel121:
	movl	16(%esp), %eax                              # SrcLine 264
	movl	%eax, 12(%esp)                              # SrcLine 264
	movl	%eax, 28(%esp)                              # SrcLine 264
.LBB10_7:                                                   # %return
	movl	28(%esp), %eax
.Llabel113:
	addl	$36, %esp                                   # SrcLine 264
	ret                                                 # SrcLine 264
.LBB10_8:                                                   # %bb6
.Llabel122:
	movl	16(%esp), %eax                              # SrcLine 259
	movl	%eax, 8(%esp)                               # SrcLine 259
	jmp	.LBB10_5
	.size	new_RFile, .-new_RFile
.Lfunc_end10:


	.align	16
	.globl	write_fun
	.type	write_fun,@function
write_fun:                                                  # @write_fun
.Lfunc_begin11:
.Llabel127:
.LBB11_0:                                                   # %entry
	pushl	%esi
.Llabel125:
	subl	$1072, %esp
.Llabel126:
	movl	1080(%esp), %eax
	movl	%eax, 1068(%esp)
	movl	1084(%esp), %eax
	movl	%eax, 1064(%esp)
	movl	1088(%esp), %eax
	movl	%eax, 1060(%esp)
.Llabel128:
	movl	$0, 1044(%esp)                              # SrcLine 271
	leal	20(%esp), %esi
	jmp	.LBB11_9
.LBB11_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header is BB11_9
.Llabel129:
	movl	$0, 1048(%esp)                              # SrcLine 273
	jmp	.LBB11_3
.LBB11_2:                                                   # %bb1
                                                            # Loop Depth 2
                                                            # Loop Header is BB11_3
                                                            # Inner Loop
                                                            # Inside Loop BB11_9 Depth 1
.Llabel130:
	movl	1064(%esp), %eax                            # SrcLine 275
	movb	4(%eax), %al                                # SrcLine 275
	movl	1048(%esp), %ecx                            # SrcLine 275
	movb	%al, 20(%esp,%ecx)                          # SrcLine 275
	incl	1048(%esp)                                  # SrcLine 275
.Llabel131:
	movl	1064(%esp), %eax                            # SrcLine 276
	movl	8(%eax), %eax                               # SrcLine 276
	movl	%eax, 1064(%esp)                            # SrcLine 276
.LBB11_3:                                                   # %bb2
                                                            # Loop Depth 2
                                                            # Loop Header
                                                            # Inner Loop
                                                            # Inside Loop BB11_9 Depth 1
.Llabel132:
	cmpl	$0, 1064(%esp)                              # SrcLine 274
	je	.LBB11_5
.LBB11_4:                                                   # %bb3
                                                            # Loop Depth 2
                                                            # Loop Header is BB11_3
                                                            # Inner Loop
                                                            # Inside Loop BB11_9 Depth 1
	cmpl	$1024, 1048(%esp)                           # SrcLine 274
	jl	.LBB11_2                                    # SrcLine 274
.LBB11_5:                                                   # %bb4
                                                            # Loop Depth 1
                                                            # Loop Header is BB11_9
.Llabel133:
	cmpl	$1023, 1048(%esp)                           # SrcLine 278
	jg	.LBB11_7
.LBB11_6:                                                   # %bb5
                                                            # Loop Depth 1
                                                            # Loop Header is BB11_9
	movl	1048(%esp), %eax                            # SrcLine 278
	movb	$0, 20(%esp,%eax)                           # SrcLine 278
.LBB11_7:                                                   # %bb6
                                                            # Loop Depth 1
                                                            # Loop Header is BB11_9
.Llabel134:
	movl	1068(%esp), %eax                            # SrcLine 279
	movl	4(%eax), %eax                               # SrcLine 279
	movl	4(%eax), %eax                               # SrcLine 279
	movl	8(%eax), %eax                               # SrcLine 279
	movl	1048(%esp), %ecx                            # SrcLine 279
	movl	%ecx, 8(%esp)                               # SrcLine 279
	movl	%esi, 4(%esp)                               # SrcLine 279
	movl	%eax, (%esp)                                # SrcLine 279
	call	write                                       # SrcLine 279
	movl	%eax, 1052(%esp)                            # SrcLine 279
	testl	%eax, %eax                                  # SrcLine 279
	js	.LBB11_13
.LBB11_8:                                                   # %bb9
                                                            # Loop Depth 1
                                                            # Loop Header is BB11_9
.Llabel135:
	movl	1052(%esp), %eax                            # SrcLine 281
	addl	%eax, 1044(%esp)                            # SrcLine 281
.LBB11_9:                                                   # %bb10
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            #   Child Loop BB11_3 Depth 2
.Llabel136:
	cmpl	$0, 1064(%esp)                              # SrcLine 272
	jne	.LBB11_1                                    # SrcLine 272
.LBB11_10:                                                  # %bb11
.Llabel137:
	movl	1044(%esp), %eax                            # SrcLine 283
	movl	%eax, 16(%esp)                              # SrcLine 283
.LBB11_11:                                                  # %bb12
	movl	16(%esp), %eax                              # SrcLine 283
	movl	%eax, 1056(%esp)                            # SrcLine 283
.LBB11_12:                                                  # %return
	movl	1056(%esp), %eax
.Llabel124:
	addl	$1072, %esp                                 # SrcLine 283
	popl	%esi                                        # SrcLine 283
	ret                                                 # SrcLine 283
.LBB11_13:                                                  # %bb8
.Llabel138:
	movl	1044(%esp), %esi                            # SrcLine 280
	movl	%esi, 16(%esp)                              # SrcLine 280
	jmp	.LBB11_11
	.size	write_fun, .-write_fun
.Lfunc_end11:


	.align	16
	.globl	installW_fun
	.type	installW_fun,@function
installW_fun:                                               # @installW_fun
.Lfunc_begin12:
.Llabel142:
.LBB12_0:                                                   # %entry
	subl	$44, %esp
.Llabel141:
	movl	48(%esp), %eax
	movl	%eax, 40(%esp)
	movl	52(%esp), %eax
	movl	%eax, 36(%esp)
	movl	56(%esp), %eax
	movl	%eax, 32(%esp)
.Llabel143:
	movl	$envmut, (%esp)                             # SrcLine 287
	call	pthread_mutex_lock                          # SrcLine 287
.Llabel144:
	movl	40(%esp), %eax                              # SrcLine 288
	movl	4(%eax), %eax                               # SrcLine 288
	movl	4(%eax), %eax                               # SrcLine 288
	movl	8(%eax), %eax                               # SrcLine 288
	movl	%eax, 16(%esp)                              # SrcLine 288
.Llabel145:
	movl	%eax, %ecx                                  # SrcLine 289
	andl	$31, %ecx                                   # SrcLine 289
	andl	$4294967264, %eax                           # SrcLine 289
	shrl	$3, %eax                                    # SrcLine 289
	#APP
	btl %ecx,writeUsed(%eax) ; setcb %al
	#NO_APP
	movb	%al, 27(%esp)                               # SrcLine 289
	movb	%al, 11(%esp)                               # SrcLine 289
	movsbl	11(%esp), %eax                              # SrcLine 289
	movl	%eax, 20(%esp)                              # SrcLine 289
.Llabel146:
	movl	36(%esp), %eax                              # SrcLine 290
	movl	16(%esp), %ecx                              # SrcLine 290
	movl	%eax, wrTable(,%ecx,4)                      # SrcLine 290
	movl	16(%esp), %eax                              # SrcLine 290
	movl	%eax, %ecx                                  # SrcLine 290
	andl	$31, %ecx                                   # SrcLine 290
	andl	$4294967264, %eax                           # SrcLine 290
	shrl	$3, %eax                                    # SrcLine 290
	#APP
	btsl %ecx,writeUsed(%eax)
	#NO_APP
.Llabel147:
	movl	$1, envRootsDirty                           # SrcLine 291
.Llabel148:
	movl	16(%esp), %eax                              # SrcLine 292
	movl	maxDesc, %ecx                               # SrcLine 292
	cmpl	%eax, %ecx                                  # SrcLine 292
	cmovge	%ecx, %eax                                  # SrcLine 292
	movl	%eax, maxDesc                               # SrcLine 292
.Llabel149:
	cmpl	$0, eventThread                             # SrcLine 293
	je	.LBB12_5
.LBB12_1:                                                   # %bb2
.Llabel150:
	cmpl	$0, 20(%esp)                                # SrcLine 294
	jne	.LBB12_3
.LBB12_2:                                                   # %bb3
	movl	eventThread, %eax                           # SrcLine 294
	movl	12(%eax), %eax                              # SrcLine 294
	movl	%eax, (%esp)                                # SrcLine 294
	movl	$10, 4(%esp)                                # SrcLine 294
	call	pthread_kill                                # SrcLine 294
.LBB12_3:                                                   # %bb4
.Llabel151:
	movl	$envmut, (%esp)                             # SrcLine 295
	call	pthread_mutex_unlock                        # SrcLine 295
.Llabel152:
	movl	$0, 12(%esp)                                # SrcLine 296
	movl	$0, 28(%esp)                                # SrcLine 296
.LBB12_4:                                                   # %return
	movsbl	28(%esp), %eax                              # SrcLine 296
.Llabel140:
	addl	$44, %esp                                   # SrcLine 296
	ret                                                 # SrcLine 296
.LBB12_5:                                                   # %bb
.Llabel153:
	call	startLoop                                   # SrcLine 293
	jmp	.LBB12_3
	.size	installW_fun, .-installW_fun
.Lfunc_end12:


	.align	16
	.globl	new_WFile
	.type	new_WFile,@function
new_WFile:                                                  # @new_WFile
.Lfunc_begin13:
.Llabel157:
.LBB13_0:                                                   # %entry
	subl	$36, %esp
.Llabel156:
	movl	40(%esp), %eax
	movl	%eax, 32(%esp)
	.align	16
.LBB13_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel158:
	movl	hp, %eax                                    # SrcLine 300
	movl	%eax, 16(%esp)                              # SrcLine 300
	movl	lim, %eax                                   # SrcLine 300
	movl	%eax, 24(%esp)                              # SrcLine 300
	movl	16(%esp), %ecx                              # SrcLine 300
	addl	$64, %ecx                                   # SrcLine 300
	movl	%ecx, 20(%esp)                              # SrcLine 300
	movl	16(%esp), %edx                              # SrcLine 300
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 300
	cmpl	%edx, %eax                                  # SrcLine 300
	jne	.LBB13_1                                    # SrcLine 300
.LBB13_2:                                                   # %bb4
	movl	24(%esp), %eax                              # SrcLine 300
	cmpl	%eax, 20(%esp)                              # SrcLine 300
	jb	.LBB13_6
.LBB13_3:                                                   # %bb5
	movl	24(%esp), %eax                              # SrcLine 300
	cmpl	%eax, 16(%esp)                              # SrcLine 300
	jb	.LBB13_8
.LBB13_4:                                                   # %bb7
	movl	$0, 8(%esp)                                 # SrcLine 300
.LBB13_5:                                                   # %bb8
	movl	8(%esp), %eax                               # SrcLine 300
	movl	%eax, 4(%esp)                               # SrcLine 300
	movl	$16, (%esp)                                 # SrcLine 300
	call	force                                       # SrcLine 300
	movl	%eax, 16(%esp)                              # SrcLine 300
.LBB13_6:                                                   # %bb9
.Llabel159:
	movl	16(%esp), %eax                              # SrcLine 301
	movl	$__GC__WFile_POSIX, (%eax)                  # SrcLine 301
.Llabel160:
	movl	32(%esp), %eax                              # SrcLine 302
	movl	%eax, (%esp)                                # SrcLine 302
	call	new_File                                    # SrcLine 302
	movl	16(%esp), %ecx                              # SrcLine 302
	movl	%eax, 4(%ecx)                               # SrcLine 302
.Llabel161:
	movl	16(%esp), %eax                              # SrcLine 303
	movl	$write_fun, 8(%eax)                         # SrcLine 303
.Llabel162:
	movl	16(%esp), %eax                              # SrcLine 304
	movl	$installW_fun, 12(%eax)                     # SrcLine 304
.Llabel163:
	movl	16(%esp), %eax                              # SrcLine 305
	movl	%eax, 12(%esp)                              # SrcLine 305
	movl	%eax, 28(%esp)                              # SrcLine 305
.LBB13_7:                                                   # %return
	movl	28(%esp), %eax
.Llabel155:
	addl	$36, %esp                                   # SrcLine 305
	ret                                                 # SrcLine 305
.LBB13_8:                                                   # %bb6
.Llabel164:
	movl	16(%esp), %eax                              # SrcLine 300
	movl	%eax, 8(%esp)                               # SrcLine 300
	jmp	.LBB13_5
	.size	new_WFile, .-new_WFile
.Lfunc_end13:


	.align	16
	.globl	exit_fun
	.type	exit_fun,@function
exit_fun:                                                   # @exit_fun
.Lfunc_begin14:
.Llabel167:
.LBB14_0:                                                   # %entry
	subl	$20, %esp
.Llabel166:
	movl	24(%esp), %eax
	movl	%eax, 16(%esp)
	movl	28(%esp), %eax
	movl	%eax, 12(%esp)
	movl	32(%esp), %eax
	movl	%eax, 8(%esp)
.Llabel168:
	movl	$envmut, (%esp)                             # SrcLine 311
	call	pthread_mutex_lock                          # SrcLine 311
.Llabel169:
	movl	$rts, (%esp)                                # SrcLine 312
	call	pthread_mutex_lock                          # SrcLine 312
.Llabel170:
	movl	12(%esp), %eax                              # SrcLine 313
	movl	%eax, (%esp)                                # SrcLine 313
	call	exit                                        # SrcLine 313
	.size	exit_fun, .-exit_fun
.Lfunc_end14:


	.align	16
	.globl	open_fun
	.type	open_fun,@function
open_fun:                                                   # @open_fun
.Lfunc_begin15:
.Llabel174:
.LBB15_0:                                                   # %entry
	subl	$1076, %esp
.Llabel173:
	movl	1080(%esp), %eax
	movl	%eax, 1072(%esp)
	movl	1084(%esp), %eax
	movl	%eax, 1068(%esp)
.Llabel175:
	movl	$0, 1044(%esp)                              # SrcLine 318
	jmp	.LBB15_2
.LBB15_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header is BB15_2
                                                            # Inner Loop
.Llabel176:
	movl	1072(%esp), %eax                            # SrcLine 320
	movb	4(%eax), %al                                # SrcLine 320
	movl	1044(%esp), %ecx                            # SrcLine 320
	movb	%al, 20(%esp,%ecx)                          # SrcLine 320
	incl	1044(%esp)                                  # SrcLine 320
.Llabel177:
	movl	1072(%esp), %eax                            # SrcLine 321
	movl	8(%eax), %eax                               # SrcLine 321
	movl	%eax, 1072(%esp)                            # SrcLine 321
.LBB15_2:                                                   # %bb1
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel178:
	cmpl	$0, 1072(%esp)                              # SrcLine 319
	je	.LBB15_4
.LBB15_3:                                                   # %bb2
                                                            # Loop Depth 1
                                                            # Loop Header is BB15_2
                                                            # Inner Loop
	cmpl	$1024, 1044(%esp)                           # SrcLine 319
	jl	.LBB15_1                                    # SrcLine 319
.LBB15_4:                                                   # %bb3
.Llabel179:
	movl	1044(%esp), %eax                            # SrcLine 323
	movb	$0, 20(%esp,%eax)                           # SrcLine 323
.Llabel180:
	movl	1068(%esp), %eax                            # SrcLine 324
	movl	%eax, 4(%esp)                               # SrcLine 324
	leal	20(%esp), %eax
	movl	%eax, (%esp)                                # SrcLine 324
	movl	$420, 8(%esp)                               # SrcLine 324
	call	open                                        # SrcLine 324
	movl	%eax, 1048(%esp)                            # SrcLine 324
	testl	%eax, %eax                                  # SrcLine 324
	js	.LBB15_13
	.align	16
.LBB15_5:                                                   # %bb6
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel181:
	movl	hp, %eax                                    # SrcLine 326
	movl	%eax, 1052(%esp)                            # SrcLine 326
	movl	lim, %eax                                   # SrcLine 326
	movl	%eax, 1060(%esp)                            # SrcLine 326
	movl	1052(%esp), %ecx                            # SrcLine 326
	addl	$8, %ecx                                    # SrcLine 326
	movl	%ecx, 1056(%esp)                            # SrcLine 326
	movl	1052(%esp), %edx                            # SrcLine 326
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 326
	cmpl	%edx, %eax                                  # SrcLine 326
	jne	.LBB15_5                                    # SrcLine 326
.LBB15_6:                                                   # %bb10
	movl	1060(%esp), %eax                            # SrcLine 326
	cmpl	%eax, 1056(%esp)                            # SrcLine 326
	jb	.LBB15_10
.LBB15_7:                                                   # %bb11
	movl	1060(%esp), %eax                            # SrcLine 326
	cmpl	%eax, 1052(%esp)                            # SrcLine 326
	jb	.LBB15_14
.LBB15_8:                                                   # %bb13
	movl	$0, 16(%esp)                                # SrcLine 326
.LBB15_9:                                                   # %bb14
	movl	16(%esp), %eax                              # SrcLine 326
	movl	%eax, 4(%esp)                               # SrcLine 326
	movl	$2, (%esp)                                  # SrcLine 326
	call	force                                       # SrcLine 326
	movl	%eax, 1052(%esp)                            # SrcLine 326
.LBB15_10:                                                  # %bb15
.Llabel182:
	movl	1052(%esp), %eax                            # SrcLine 327
	movl	$__GC___Just_Prelude, (%eax)                # SrcLine 327
.Llabel183:
	movl	1048(%esp), %eax                            # SrcLine 328
	movl	%eax, (%esp)                                # SrcLine 328
	call	new_File                                    # SrcLine 328
	movl	1052(%esp), %ecx                            # SrcLine 328
	movl	%eax, 4(%ecx)                               # SrcLine 328
.Llabel184:
	movl	1052(%esp), %eax                            # SrcLine 329
	movl	%eax, 12(%esp)                              # SrcLine 329
.LBB15_11:                                                  # %bb16
	movl	12(%esp), %eax                              # SrcLine 329
	movl	%eax, 1064(%esp)                            # SrcLine 329
.LBB15_12:                                                  # %return
	movl	1064(%esp), %eax
.Llabel172:
	addl	$1076, %esp                                 # SrcLine 329
	ret                                                 # SrcLine 329
.LBB15_13:                                                  # %bb5
.Llabel185:
	movl	$0, 12(%esp)                                # SrcLine 325
	jmp	.LBB15_11
.LBB15_14:                                                  # %bb12
.Llabel186:
	movl	1052(%esp), %eax                            # SrcLine 326
	movl	%eax, 16(%esp)                              # SrcLine 326
	jmp	.LBB15_9
	.size	open_fun, .-open_fun
.Lfunc_end15:


	.align	16
	.globl	openR_fun
	.type	openR_fun,@function
openR_fun:                                                  # @openR_fun
.Lfunc_begin16:
.Llabel190:
.LBB16_0:                                                   # %entry
	subl	$52, %esp
.Llabel189:
	movl	56(%esp), %eax
	movl	%eax, 48(%esp)
	movl	60(%esp), %eax
	movl	%eax, 44(%esp)
	movl	64(%esp), %eax
	movl	%eax, 40(%esp)
.Llabel191:
	movl	$envmut, (%esp)                             # SrcLine 334
	call	pthread_mutex_lock                          # SrcLine 334
.Llabel192:
	movl	44(%esp), %eax                              # SrcLine 335
	movl	%eax, (%esp)                                # SrcLine 335
	movl	$0, 4(%esp)                                 # SrcLine 335
	call	open_fun                                    # SrcLine 335
	movl	%eax, 20(%esp)                              # SrcLine 335
	testl	%eax, %eax                                  # SrcLine 335
	je	.LBB16_7
	.align	16
.LBB16_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel193:
	movl	hp, %eax                                    # SrcLine 337
	movl	%eax, 24(%esp)                              # SrcLine 337
	movl	lim, %eax                                   # SrcLine 337
	movl	%eax, 32(%esp)                              # SrcLine 337
	movl	24(%esp), %ecx                              # SrcLine 337
	addl	$16, %ecx                                   # SrcLine 337
	movl	%ecx, 28(%esp)                              # SrcLine 337
	movl	24(%esp), %edx                              # SrcLine 337
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 337
	cmpl	%edx, %eax                                  # SrcLine 337
	jne	.LBB16_1                                    # SrcLine 337
.LBB16_2:                                                   # %bb4
	movl	32(%esp), %eax                              # SrcLine 337
	cmpl	%eax, 28(%esp)                              # SrcLine 337
	jb	.LBB16_6
.LBB16_3:                                                   # %bb5
	movl	32(%esp), %eax                              # SrcLine 337
	cmpl	%eax, 24(%esp)                              # SrcLine 337
	jb	.LBB16_9
.LBB16_4:                                                   # %bb7
	movl	$0, 12(%esp)                                # SrcLine 337
.LBB16_5:                                                   # %bb8
	movl	12(%esp), %eax                              # SrcLine 337
	movl	%eax, 4(%esp)                               # SrcLine 337
	movl	$4, (%esp)                                  # SrcLine 337
	call	force                                       # SrcLine 337
	movl	%eax, 24(%esp)                              # SrcLine 337
.LBB16_6:                                                   # %bb9
.Llabel194:
	movl	24(%esp), %eax                              # SrcLine 338
	movl	$__GC__RFile_POSIX, (%eax)                  # SrcLine 338
.Llabel195:
	movl	20(%esp), %eax                              # SrcLine 339
	movl	4(%eax), %eax                               # SrcLine 339
	movl	24(%esp), %ecx                              # SrcLine 339
	movl	%eax, 4(%ecx)                               # SrcLine 339
.Llabel196:
	movl	24(%esp), %eax                              # SrcLine 340
	movl	$read_fun, 8(%eax)                          # SrcLine 340
.Llabel197:
	movl	24(%esp), %eax                              # SrcLine 341
	movl	$installR_fun, 12(%eax)                     # SrcLine 341
.Llabel198:
	movl	24(%esp), %eax                              # SrcLine 342
	movl	20(%esp), %ecx                              # SrcLine 342
	movl	%eax, 4(%ecx)                               # SrcLine 342
.LBB16_7:                                                   # %bb10
.Llabel199:
	movl	$envmut, (%esp)                             # SrcLine 344
	call	pthread_mutex_unlock                        # SrcLine 344
.Llabel200:
	movl	20(%esp), %eax                              # SrcLine 345
	movl	%eax, 16(%esp)                              # SrcLine 345
	movl	%eax, 36(%esp)                              # SrcLine 345
.LBB16_8:                                                   # %return
	movl	36(%esp), %eax
.Llabel188:
	addl	$52, %esp                                   # SrcLine 345
	ret                                                 # SrcLine 345
.LBB16_9:                                                   # %bb6
.Llabel201:
	movl	24(%esp), %eax                              # SrcLine 337
	movl	%eax, 12(%esp)                              # SrcLine 337
	jmp	.LBB16_5
	.size	openR_fun, .-openR_fun
.Lfunc_end16:


	.align	16
	.globl	openW_fun
	.type	openW_fun,@function
openW_fun:                                                  # @openW_fun
.Lfunc_begin17:
.Llabel205:
.LBB17_0:                                                   # %entry
	subl	$52, %esp
.Llabel204:
	movl	56(%esp), %eax
	movl	%eax, 48(%esp)
	movl	60(%esp), %eax
	movl	%eax, 44(%esp)
	movl	64(%esp), %eax
	movl	%eax, 40(%esp)
.Llabel206:
	movl	$envmut, (%esp)                             # SrcLine 349
	call	pthread_mutex_lock                          # SrcLine 349
.Llabel207:
	movl	44(%esp), %eax                              # SrcLine 350
	movl	%eax, (%esp)                                # SrcLine 350
	movl	$577, 4(%esp)                               # SrcLine 350
	call	open_fun                                    # SrcLine 350
	movl	%eax, 20(%esp)                              # SrcLine 350
	testl	%eax, %eax                                  # SrcLine 350
	je	.LBB17_7
	.align	16
.LBB17_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel208:
	movl	hp, %eax                                    # SrcLine 352
	movl	%eax, 24(%esp)                              # SrcLine 352
	movl	lim, %eax                                   # SrcLine 352
	movl	%eax, 32(%esp)                              # SrcLine 352
	movl	24(%esp), %ecx                              # SrcLine 352
	addl	$16, %ecx                                   # SrcLine 352
	movl	%ecx, 28(%esp)                              # SrcLine 352
	movl	24(%esp), %edx                              # SrcLine 352
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 352
	cmpl	%edx, %eax                                  # SrcLine 352
	jne	.LBB17_1                                    # SrcLine 352
.LBB17_2:                                                   # %bb4
	movl	32(%esp), %eax                              # SrcLine 352
	cmpl	%eax, 28(%esp)                              # SrcLine 352
	jb	.LBB17_6
.LBB17_3:                                                   # %bb5
	movl	32(%esp), %eax                              # SrcLine 352
	cmpl	%eax, 24(%esp)                              # SrcLine 352
	jb	.LBB17_9
.LBB17_4:                                                   # %bb7
	movl	$0, 12(%esp)                                # SrcLine 352
.LBB17_5:                                                   # %bb8
	movl	12(%esp), %eax                              # SrcLine 352
	movl	%eax, 4(%esp)                               # SrcLine 352
	movl	$4, (%esp)                                  # SrcLine 352
	call	force                                       # SrcLine 352
	movl	%eax, 24(%esp)                              # SrcLine 352
.LBB17_6:                                                   # %bb9
.Llabel209:
	movl	24(%esp), %eax                              # SrcLine 353
	movl	$__GC__WFile_POSIX, (%eax)                  # SrcLine 353
.Llabel210:
	movl	20(%esp), %eax                              # SrcLine 354
	movl	4(%eax), %eax                               # SrcLine 354
	movl	24(%esp), %ecx                              # SrcLine 354
	movl	%eax, 4(%ecx)                               # SrcLine 354
.Llabel211:
	movl	24(%esp), %eax                              # SrcLine 355
	movl	$write_fun, 8(%eax)                         # SrcLine 355
.Llabel212:
	movl	24(%esp), %eax                              # SrcLine 356
	movl	$installW_fun, 12(%eax)                     # SrcLine 356
.Llabel213:
	movl	24(%esp), %eax                              # SrcLine 357
	movl	20(%esp), %ecx                              # SrcLine 357
	movl	%eax, 4(%ecx)                               # SrcLine 357
.LBB17_7:                                                   # %bb10
.Llabel214:
	movl	$envmut, (%esp)                             # SrcLine 359
	call	pthread_mutex_unlock                        # SrcLine 359
.Llabel215:
	movl	20(%esp), %eax                              # SrcLine 360
	movl	%eax, 16(%esp)                              # SrcLine 360
	movl	%eax, 36(%esp)                              # SrcLine 360
.LBB17_8:                                                   # %return
	movl	36(%esp), %eax
.Llabel203:
	addl	$52, %esp                                   # SrcLine 360
	ret                                                 # SrcLine 360
.LBB17_9:                                                   # %bb6
.Llabel216:
	movl	24(%esp), %eax                              # SrcLine 352
	movl	%eax, 12(%esp)                              # SrcLine 352
	jmp	.LBB17_5
	.size	openW_fun, .-openW_fun
.Lfunc_end17:


	.align	16
	.globl	new_Socket
	.type	new_Socket,@function
new_Socket:                                                 # @new_Socket
.Lfunc_begin18:
.Llabel220:
.LBB18_0:                                                   # %entry
	subl	$68, %esp
.Llabel219:
	movl	72(%esp), %eax
	movl	%eax, 64(%esp)
	.align	16
.LBB18_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel221:
	movl	hp, %eax                                    # SrcLine 367
	movl	%eax, 28(%esp)                              # SrcLine 367
	movl	lim, %eax                                   # SrcLine 367
	movl	%eax, 56(%esp)                              # SrcLine 367
	movl	28(%esp), %ecx                              # SrcLine 367
	addl	$24, %ecx                                   # SrcLine 367
	movl	%ecx, 52(%esp)                              # SrcLine 367
	movl	28(%esp), %edx                              # SrcLine 367
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 367
	cmpl	%edx, %eax                                  # SrcLine 367
	jne	.LBB18_1                                    # SrcLine 367
.LBB18_2:                                                   # %bb4
	movl	56(%esp), %eax                              # SrcLine 367
	cmpl	%eax, 52(%esp)                              # SrcLine 367
	jb	.LBB18_6
.LBB18_3:                                                   # %bb5
	movl	56(%esp), %eax                              # SrcLine 367
	cmpl	%eax, 28(%esp)                              # SrcLine 367
	jb	.LBB18_8
.LBB18_4:                                                   # %bb7
	movl	$0, 20(%esp)                                # SrcLine 367
.LBB18_5:                                                   # %bb8
	movl	20(%esp), %eax                              # SrcLine 367
	movl	%eax, 4(%esp)                               # SrcLine 367
	movl	$6, (%esp)                                  # SrcLine 367
	call	force                                       # SrcLine 367
	movl	%eax, 28(%esp)                              # SrcLine 367
.LBB18_6:                                                   # %bb9
.Llabel222:
	movl	28(%esp), %eax                              # SrcLine 368
	movl	$__GC__Socket_POSIX, (%eax)                 # SrcLine 368
.Llabel223:
	movl	64(%esp), %eax                              # SrcLine 369
	movl	%eax, (%esp)                                # SrcLine 369
	call	new_Closable                                # SrcLine 369
	movl	28(%esp), %ecx                              # SrcLine 369
	movl	%eax, 4(%ecx)                               # SrcLine 369
.Llabel224:
	movl	64(%esp), %eax                              # SrcLine 370
	movl	%eax, (%esp)                                # SrcLine 370
	call	new_RFile                                   # SrcLine 370
	movl	28(%esp), %ecx                              # SrcLine 370
	movl	%eax, 16(%ecx)                              # SrcLine 370
.Llabel225:
	movl	64(%esp), %eax                              # SrcLine 371
	movl	%eax, (%esp)                                # SrcLine 371
	call	new_WFile                                   # SrcLine 371
	movl	28(%esp), %ecx                              # SrcLine 371
	movl	%eax, 20(%ecx)                              # SrcLine 371
.Llabel226:
	movl	64(%esp), %eax                              # SrcLine 372
	movl	sockTable(,%eax,4), %eax                    # SrcLine 372
	movl	16(%eax), %ecx                              # SrcLine 372
	movl	%ecx, 44(%esp)                              # SrcLine 372
	movl	12(%eax), %ecx                              # SrcLine 372
	movl	%ecx, 40(%esp)                              # SrcLine 372
	movl	8(%eax), %ecx                               # SrcLine 372
	movl	%ecx, 36(%esp)                              # SrcLine 372
	movl	4(%eax), %eax                               # SrcLine 372
	movl	%eax, 32(%esp)                              # SrcLine 372
.Llabel227:
	movl	44(%esp), %eax                              # SrcLine 373
	movl	%eax, 12(%esp)                              # SrcLine 373
	movl	40(%esp), %eax                              # SrcLine 373
	movl	%eax, 8(%esp)                               # SrcLine 373
	movl	36(%esp), %eax                              # SrcLine 373
	movl	%eax, 4(%esp)                               # SrcLine 373
	movl	32(%esp), %eax                              # SrcLine 373
	movl	%eax, (%esp)                                # SrcLine 373
	call	mkHost                                      # SrcLine 373
	movl	28(%esp), %ecx                              # SrcLine 373
	movl	%eax, 8(%ecx)                               # SrcLine 373
.Llabel228:
	movl	44(%esp), %eax                              # SrcLine 374
	movl	%eax, 12(%esp)                              # SrcLine 374
	movl	40(%esp), %eax                              # SrcLine 374
	movl	%eax, 8(%esp)                               # SrcLine 374
	movl	36(%esp), %eax                              # SrcLine 374
	movl	%eax, 4(%esp)                               # SrcLine 374
	movl	32(%esp), %eax                              # SrcLine 374
	movl	%eax, (%esp)                                # SrcLine 374
	call	mkPort                                      # SrcLine 374
	movl	28(%esp), %ecx                              # SrcLine 374
	movl	%eax, 12(%ecx)                              # SrcLine 374
.Llabel229:
	movl	28(%esp), %eax                              # SrcLine 375
	movl	%eax, 24(%esp)                              # SrcLine 375
	movl	%eax, 60(%esp)                              # SrcLine 375
.LBB18_7:                                                   # %return
	movl	60(%esp), %eax
.Llabel218:
	addl	$68, %esp                                   # SrcLine 375
	ret                                                 # SrcLine 375
.LBB18_8:                                                   # %bb6
.Llabel230:
	movl	28(%esp), %eax                              # SrcLine 367
	movl	%eax, 20(%esp)                              # SrcLine 367
	jmp	.LBB18_5
	.size	new_Socket, .-new_Socket
.Lfunc_end18:


	.align	16
	.globl	new_socket
	.type	new_socket,@function
new_socket:                                                 # @new_socket
.Lfunc_begin19:
.Llabel234:
.LBB19_0:                                                   # %entry
	subl	$44, %esp
.Llabel233:
	movl	48(%esp), %eax
	movl	%eax, 40(%esp)
.Llabel235:
	movl	$0, 8(%esp)                                 # SrcLine 381
	movl	$1, 4(%esp)                                 # SrcLine 381
	movl	$2, (%esp)                                  # SrcLine 381
	call	socket                                      # SrcLine 381
	movl	%eax, 24(%esp)                              # SrcLine 381
.Llabel236:
	movl	%eax, (%esp)                                # SrcLine 382
	movl	$2048, 8(%esp)                              # SrcLine 382
	movl	$4, 4(%esp)                                 # SrcLine 382
	call	fcntl                                       # SrcLine 382
.Llabel237:
	movl	24(%esp), %eax                              # SrcLine 383
	movl	maxDesc, %ecx                               # SrcLine 383
	cmpl	%eax, %ecx                                  # SrcLine 383
	cmovge	%ecx, %eax                                  # SrcLine 383
	movl	%eax, maxDesc                               # SrcLine 383
	.align	16
.LBB19_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel238:
	movl	hp, %eax                                    # SrcLine 384
	movl	%eax, 20(%esp)                              # SrcLine 384
	movl	lim, %eax                                   # SrcLine 384
	movl	%eax, 32(%esp)                              # SrcLine 384
	movl	20(%esp), %ecx                              # SrcLine 384
	addl	$28, %ecx                                   # SrcLine 384
	movl	%ecx, 28(%esp)                              # SrcLine 384
	movl	20(%esp), %edx                              # SrcLine 384
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 384
	cmpl	%edx, %eax                                  # SrcLine 384
	jne	.LBB19_1                                    # SrcLine 384
.LBB19_2:                                                   # %bb5
	movl	32(%esp), %eax                              # SrcLine 384
	cmpl	%eax, 28(%esp)                              # SrcLine 384
	jb	.LBB19_6
.LBB19_3:                                                   # %bb6
	movl	32(%esp), %eax                              # SrcLine 384
	cmpl	%eax, 20(%esp)                              # SrcLine 384
	jb	.LBB19_8
.LBB19_4:                                                   # %bb8
	movl	$0, 12(%esp)                                # SrcLine 384
.LBB19_5:                                                   # %bb9
	movl	12(%esp), %eax                              # SrcLine 384
	movl	%eax, 4(%esp)                               # SrcLine 384
	movl	$7, (%esp)                                  # SrcLine 384
	call	force                                       # SrcLine 384
	movl	%eax, 20(%esp)                              # SrcLine 384
.LBB19_6:                                                   # %bb10
.Llabel239:
	movl	20(%esp), %eax                              # SrcLine 385
	movl	$__GC__SockData, (%eax)                     # SrcLine 385
.Llabel240:
	movl	40(%esp), %eax                              # SrcLine 386
	movl	20(%esp), %ecx                              # SrcLine 386
	movl	%eax, 20(%ecx)                              # SrcLine 386
.Llabel241:
	movl	20(%esp), %eax                              # SrcLine 387
	movl	24(%esp), %ecx                              # SrcLine 387
	movl	%eax, sockTable(,%ecx,4)                    # SrcLine 387
.Llabel242:
	movl	$1, envRootsDirty                           # SrcLine 388
.Llabel243:
	movl	24(%esp), %eax                              # SrcLine 389
	movl	%eax, 16(%esp)                              # SrcLine 389
	movl	%eax, 36(%esp)                              # SrcLine 389
.LBB19_7:                                                   # %return
	movl	36(%esp), %eax
.Llabel232:
	addl	$44, %esp                                   # SrcLine 389
	ret                                                 # SrcLine 389
.LBB19_8:                                                   # %bb7
.Llabel244:
	movl	20(%esp), %eax                              # SrcLine 384
	movl	%eax, 12(%esp)                              # SrcLine 384
	jmp	.LBB19_5
	.size	new_socket, .-new_socket
.Lfunc_end19:


	.align	16
	.globl	netError
	.type	netError,@function
netError:                                                   # @netError
.Lfunc_begin20:
.Llabel249:
.LBB20_0:                                                   # %entry
	pushl	%esi
.Llabel247:
	subl	$32, %esp
.Llabel248:
	movl	40(%esp), %eax
	movl	%eax, 28(%esp)
	movl	44(%esp), %eax
	movl	%eax, 24(%esp)
.Llabel250:
	movl	28(%esp), %eax                              # SrcLine 393
	movl	sockTable(,%eax,4), %eax                    # SrcLine 393
	movl	20(%eax), %eax                              # SrcLine 393
	movl	%eax, 16(%esp)                              # SrcLine 393
.Llabel251:
	movl	4(%eax), %esi                               # SrcLine 394
	movl	28(%esp), %eax                              # SrcLine 394
	movl	%eax, (%esp)                                # SrcLine 394
	call	new_Socket                                  # SrcLine 394
	movl	16(%esp), %ecx                              # SrcLine 394
	movl	%eax, 4(%esp)                               # SrcLine 394
	movl	%ecx, (%esp)                                # SrcLine 394
	movl	$0, 8(%esp)                                 # SrcLine 394
	call	*%esi                                       # SrcLine 394
	movl	%eax, 20(%esp)                              # SrcLine 394
.Llabel252:
	movl	$1, envRootsDirty                           # SrcLine 395
.Llabel253:
	movl	20(%esp), %eax                              # SrcLine 396
	movl	12(%eax), %esi                              # SrcLine 396
	movl	24(%esp), %eax                              # SrcLine 396
	movl	%eax, (%esp)                                # SrcLine 396
	call	getStr                                      # SrcLine 396
	movl	20(%esp), %ecx                              # SrcLine 396
	movl	%eax, 4(%esp)                               # SrcLine 396
	movl	%ecx, (%esp)                                # SrcLine 396
	movl	$0, 12(%esp)                                # SrcLine 396
	movl	$0, 8(%esp)                                 # SrcLine 396
	call	*%esi                                       # SrcLine 396
.LBB20_1:                                                   # %return
.Llabel254:
.Llabel246:
	addl	$32, %esp                                   # SrcLine 397
	popl	%esi                                        # SrcLine 397
	ret                                                 # SrcLine 397
	.size	netError, .-netError
.Lfunc_end20:


	.align	16
	.globl	setupConnection
	.type	setupConnection,@function
setupConnection:                                            # @setupConnection
.Lfunc_begin21:
.Llabel259:
.LBB21_0:                                                   # %entry
	pushl	%esi
.Llabel257:
	subl	$24, %esp
.Llabel258:
	movl	32(%esp), %eax
	movl	%eax, 20(%esp)
.Llabel260:
	movl	sockTable(,%eax,4), %eax                    # SrcLine 400
	movl	20(%eax), %eax                              # SrcLine 400
	movl	%eax, 12(%esp)                              # SrcLine 400
.Llabel261:
	movl	4(%eax), %esi                               # SrcLine 401
	movl	20(%esp), %eax                              # SrcLine 401
	movl	%eax, (%esp)                                # SrcLine 401
	call	new_Socket                                  # SrcLine 401
	movl	12(%esp), %ecx                              # SrcLine 401
	movl	%eax, 4(%esp)                               # SrcLine 401
	movl	%ecx, (%esp)                                # SrcLine 401
	movl	$0, 8(%esp)                                 # SrcLine 401
	call	*%esi                                       # SrcLine 401
	movl	%eax, 16(%esp)                              # SrcLine 401
.Llabel262:
	movl	20(%esp), %ecx                              # SrcLine 402
	movl	sockTable(,%ecx,4), %ecx                    # SrcLine 402
	movl	%eax, 24(%ecx)                              # SrcLine 402
.Llabel263:
	movl	$1, envRootsDirty                           # SrcLine 403
.Llabel264:
	movl	16(%esp), %eax                              # SrcLine 404
	movl	%eax, (%esp)                                # SrcLine 404
	movl	$0, 8(%esp)                                 # SrcLine 404
	movl	$0, 4(%esp)                                 # SrcLine 404
	call	*8(%eax)                                    # SrcLine 404
.LBB21_1:                                                   # %return
.Llabel265:
.Llabel256:
	addl	$24, %esp                                   # SrcLine 405
	popl	%esi                                        # SrcLine 405
	ret                                                 # SrcLine 405
	.size	setupConnection, .-setupConnection
.Lfunc_end21:


	.align	16
	.globl	mkAddr
	.type	mkAddr,@function
mkAddr:                                                     # @mkAddr
.Lfunc_begin22:
.Llabel269:
.LBB22_0:                                                   # %entry
	subl	$1068, %esp
.Llabel268:
	movl	1072(%esp), %eax
	movl	%eax, 1064(%esp)
	movl	1076(%esp), %eax
	movl	%eax, 1060(%esp)
	movl	1080(%esp), %eax
	movl	%eax, 1056(%esp)
.Llabel270:
	movl	1060(%esp), %eax                            # SrcLine 408
	movl	4(%eax), %eax                               # SrcLine 408
	movl	%eax, 12(%esp)                              # SrcLine 408
.Llabel271:
	movl	$0, 1040(%esp)                              # SrcLine 410
	jmp	.LBB22_2
.LBB22_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header is BB22_2
                                                            # Inner Loop
.Llabel272:
	movl	12(%esp), %eax                              # SrcLine 414
	movb	4(%eax), %al                                # SrcLine 414
	movl	1040(%esp), %ecx                            # SrcLine 414
	movb	%al, 16(%esp,%ecx)                          # SrcLine 414
	incl	1040(%esp)                                  # SrcLine 414
.Llabel273:
	movl	12(%esp), %eax                              # SrcLine 415
	movl	8(%eax), %eax                               # SrcLine 415
	movl	%eax, 12(%esp)                              # SrcLine 415
.LBB22_2:                                                   # %bb1
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel274:
	cmpl	$0, 12(%esp)                                # SrcLine 413
	je	.LBB22_4
.LBB22_3:                                                   # %bb2
                                                            # Loop Depth 1
                                                            # Loop Header is BB22_2
                                                            # Inner Loop
	cmpl	$1024, 1040(%esp)                           # SrcLine 413
	jl	.LBB22_1                                    # SrcLine 413
.LBB22_4:                                                   # %bb3
.Llabel275:
	movl	1040(%esp), %eax                            # SrcLine 417
	movb	$0, 16(%esp,%eax)                           # SrcLine 417
	leal	16(%esp), %eax
.Llabel276:
	movl	%eax, (%esp)                                # SrcLine 419
	call	gethostbyname                               # SrcLine 419
	movl	%eax, 1048(%esp)                            # SrcLine 419
	testl	%eax, %eax                                  # SrcLine 419
	je	.LBB22_8
.LBB22_5:                                                   # %bb6
.Llabel277:
	movl	1048(%esp), %eax                            # SrcLine 425
	movl	16(%eax), %eax                              # SrcLine 425
	movl	(%eax), %eax                                # SrcLine 425
	movl	(%eax), %eax                                # SrcLine 425
	movl	%eax, 1044(%esp)                            # SrcLine 425
.Llabel278:
	movl	1056(%esp), %ecx                            # SrcLine 426
	movl	%eax, (%ecx)                                # SrcLine 426
.Llabel279:
	movl	$0, 8(%esp)                                 # SrcLine 427
.LBB22_6:                                                   # %bb8
	movl	8(%esp), %eax                               # SrcLine 427
	movl	%eax, 1052(%esp)                            # SrcLine 427
.LBB22_7:                                                   # %return
	movl	1052(%esp), %eax
.Llabel267:
	addl	$1068, %esp                                 # SrcLine 427
	ret                                                 # SrcLine 427
.LBB22_8:                                                   # %bb5
.Llabel280:
	movl	1064(%esp), %eax                            # SrcLine 421
	movl	%eax, (%esp)                                # SrcLine 421
	movl	$.L.str897, 4(%esp)                         # SrcLine 421
	call	netError                                    # SrcLine 421
.Llabel281:
	movl	$4294967295, 8(%esp)                        # SrcLine 422
	jmp	.LBB22_6
	.size	mkAddr, .-mkAddr
.Lfunc_end22:


	.align	16
	.globl	connect_fun
	.type	connect_fun,@function
connect_fun:                                                # @connect_fun
.Lfunc_begin23:
.Llabel285:
.LBB23_0:                                                   # %entry
	subl	$76, %esp
.Llabel284:
	movl	80(%esp), %eax
	movl	%eax, 72(%esp)
	movl	84(%esp), %eax
	movl	%eax, 68(%esp)
	movl	88(%esp), %eax
	movl	%eax, 64(%esp)
	movl	92(%esp), %eax
	movl	%eax, 60(%esp)
	movl	96(%esp), %eax
	movl	%eax, 56(%esp)
.Llabel286:
	movl	$envmut, (%esp)                             # SrcLine 432
	call	pthread_mutex_lock                          # SrcLine 432
.Llabel287:
	movl	60(%esp), %eax                              # SrcLine 435
	movl	%eax, (%esp)                                # SrcLine 435
	call	new_socket                                  # SrcLine 435
	movl	%eax, 48(%esp)                              # SrcLine 435
.Llabel288:
	movl	68(%esp), %ecx                              # SrcLine 436
	leal	40(%esp), %edx
	movl	%edx, 8(%esp)                               # SrcLine 436
	movl	%ecx, 4(%esp)                               # SrcLine 436
	movl	%eax, (%esp)                                # SrcLine 436
	call	mkAddr                                      # SrcLine 436
	testl	%eax, %eax                                  # SrcLine 436
	jne	.LBB23_8
.LBB23_1:                                                   # %bb
.Llabel289:
	movl	40(%esp), %eax                              # SrcLine 437
	movl	%eax, 28(%esp)                              # SrcLine 437
.Llabel290:
	movl	64(%esp), %eax                              # SrcLine 438
	movzwl	4(%eax), %eax                               # SrcLine 438
	movl	%eax, (%esp)                                # SrcLine 438
	call	htons                                       # SrcLine 438
	movw	%ax, 26(%esp)                               # SrcLine 438
.Llabel291:
	movw	$2, 24(%esp)                                # SrcLine 439
.Llabel292:
	movl	48(%esp), %eax                              # SrcLine 440
	movl	sockTable(,%eax,4), %eax                    # SrcLine 440
	movl	36(%esp), %ecx                              # SrcLine 440
	movl	%ecx, 16(%eax)                              # SrcLine 440
	movl	32(%esp), %ecx                              # SrcLine 440
	movl	%ecx, 12(%eax)                              # SrcLine 440
	movl	28(%esp), %ecx                              # SrcLine 440
	movl	%ecx, 8(%eax)                               # SrcLine 440
	movl	24(%esp), %ecx                              # SrcLine 440
	movl	%ecx, 4(%eax)                               # SrcLine 440
	leal	24(%esp), %eax
.Llabel293:
	movl	%eax, 16(%esp)                              # SrcLine 441
	movl	48(%esp), %ecx                              # SrcLine 441
	movl	%eax, 4(%esp)                               # SrcLine 441
	movl	%ecx, (%esp)                                # SrcLine 441
	movl	$16, 8(%esp)                                # SrcLine 441
	call	connect                                     # SrcLine 441
	testl	%eax, %eax                                  # SrcLine 441
	jns	.LBB23_5                                    # SrcLine 441
.LBB23_2:                                                   # %bb4
.Llabel294:
	call	__errno_location                            # SrcLine 442
	cmpl	$115, (%eax)                                # SrcLine 442
	jne	.LBB23_4                                    # SrcLine 442
.LBB23_3:                                                   # %bb5
.Llabel295:
	movl	48(%esp), %eax                              # SrcLine 443
	movl	%eax, %ecx                                  # SrcLine 443
	andl	$31, %ecx                                   # SrcLine 443
	andl	$4294967264, %eax                           # SrcLine 443
	shrl	$3, %eax                                    # SrcLine 443
	#APP
	btsl %ecx,writeUsed(%eax)
	#NO_APP
	jmp	.LBB23_6
.LBB23_4:                                                   # %bb6
.Llabel296:
	movl	48(%esp), %eax                              # SrcLine 445
	movl	%eax, (%esp)                                # SrcLine 445
	movl	$.L.str909, 4(%esp)                         # SrcLine 445
	call	netError                                    # SrcLine 445
	jmp	.LBB23_6
.LBB23_5:                                                   # %bb8
.Llabel297:
	movl	48(%esp), %eax                              # SrcLine 448
	movl	%eax, (%esp)                                # SrcLine 448
	call	setupConnection                             # SrcLine 448
.LBB23_6:                                                   # %bb9
.Llabel298:
	cmpl	$0, eventThread                             # SrcLine 450
	je	.LBB23_10
.LBB23_7:                                                   # %bb11
.Llabel299:
	movl	eventThread, %eax                           # SrcLine 451
	movl	12(%eax), %eax                              # SrcLine 451
	movl	%eax, (%esp)                                # SrcLine 451
	movl	$10, 4(%esp)                                # SrcLine 451
	call	pthread_kill                                # SrcLine 451
.LBB23_8:                                                   # %bb12
.Llabel300:
	movl	$envmut, (%esp)                             # SrcLine 453
	call	pthread_mutex_unlock                        # SrcLine 453
.Llabel301:
	movl	$0, 20(%esp)                                # SrcLine 454
	movl	$0, 52(%esp)                                # SrcLine 454
.LBB23_9:                                                   # %return
	movsbl	52(%esp), %eax                              # SrcLine 454
.Llabel283:
	addl	$76, %esp                                   # SrcLine 454
	ret                                                 # SrcLine 454
.LBB23_10:                                                  # %bb10
.Llabel302:
	call	startLoop                                   # SrcLine 450
	jmp	.LBB23_8
	.size	connect_fun, .-connect_fun
.Lfunc_end23:


	.align	16
	.globl	listen_fun
	.type	listen_fun,@function
listen_fun:                                                 # @listen_fun
.Lfunc_begin24:
.Llabel306:
.LBB24_0:                                                   # %entry
	subl	$68, %esp
.Llabel305:
	movl	72(%esp), %eax
	movl	%eax, 64(%esp)
	movl	76(%esp), %eax
	movl	%eax, 60(%esp)
	movl	80(%esp), %eax
	movl	%eax, 56(%esp)
	movl	84(%esp), %eax
	movl	%eax, 52(%esp)
.Llabel307:
	movl	$envmut, (%esp)                             # SrcLine 459
	call	pthread_mutex_lock                          # SrcLine 459
.Llabel308:
	movl	56(%esp), %eax                              # SrcLine 461
	movl	%eax, (%esp)                                # SrcLine 461
	call	new_socket                                  # SrcLine 461
	movl	%eax, 44(%esp)                              # SrcLine 461
.Llabel309:
	movl	$0, 28(%esp)                                # SrcLine 462
.Llabel310:
	movl	60(%esp), %eax                              # SrcLine 463
	movzwl	4(%eax), %eax                               # SrcLine 463
	movl	%eax, (%esp)                                # SrcLine 463
	call	htons                                       # SrcLine 463
	movw	%ax, 26(%esp)                               # SrcLine 463
.Llabel311:
	movw	$2, 24(%esp)                                # SrcLine 464
	leal	24(%esp), %eax
.Llabel312:
	movl	%eax, 16(%esp)                              # SrcLine 465
	movl	44(%esp), %ecx                              # SrcLine 465
	movl	%eax, 4(%esp)                               # SrcLine 465
	movl	%ecx, (%esp)                                # SrcLine 465
	movl	$16, 8(%esp)                                # SrcLine 465
	call	bind                                        # SrcLine 465
	testl	%eax, %eax                                  # SrcLine 465
	jns	.LBB24_2
.LBB24_1:                                                   # %bb
.Llabel313:
	movl	$.L.str918, (%esp)                          # SrcLine 466
	call	perror                                      # SrcLine 466
.LBB24_2:                                                   # %bb3
.Llabel314:
	movl	44(%esp), %eax                              # SrcLine 467
	movl	%eax, (%esp)                                # SrcLine 467
	movl	$5, 4(%esp)                                 # SrcLine 467
	call	listen                                      # SrcLine 467
.Llabel315:
	movl	44(%esp), %eax                              # SrcLine 468
	movl	%eax, %ecx                                  # SrcLine 468
	andl	$31, %ecx                                   # SrcLine 468
	andl	$4294967264, %eax                           # SrcLine 468
	shrl	$3, %eax                                    # SrcLine 468
	#APP
	btsl %ecx,readUsed(%eax)
	#NO_APP
.Llabel316:
	cmpl	$0, eventThread                             # SrcLine 469
	je	.LBB24_6
.LBB24_3:                                                   # %bb5
.Llabel317:
	movl	eventThread, %eax                           # SrcLine 470
	movl	12(%eax), %eax                              # SrcLine 470
	movl	%eax, (%esp)                                # SrcLine 470
	movl	$10, 4(%esp)                                # SrcLine 470
	call	pthread_kill                                # SrcLine 470
.LBB24_4:                                                   # %bb6
.Llabel318:
	movl	$envmut, (%esp)                             # SrcLine 471
	call	pthread_mutex_unlock                        # SrcLine 471
.Llabel319:
	movl	44(%esp), %eax                              # SrcLine 472
	movl	%eax, (%esp)                                # SrcLine 472
	call	new_Closable                                # SrcLine 472
	movl	%eax, 20(%esp)                              # SrcLine 472
	movl	%eax, 48(%esp)                              # SrcLine 472
.LBB24_5:                                                   # %return
	movl	48(%esp), %eax
.Llabel304:
	addl	$68, %esp                                   # SrcLine 472
	ret                                                 # SrcLine 472
.LBB24_6:                                                   # %bb4
.Llabel320:
	call	startLoop                                   # SrcLine 469
	jmp	.LBB24_4
	.size	listen_fun, .-listen_fun
.Lfunc_end24:


	.align	16
	.globl	kill_handler
	.type	kill_handler,@function
kill_handler:                                               # @kill_handler
.Lfunc_begin25:
.Llabel323:
.LBB25_0:                                                   # %entry
.LBB25_1:                                                   # %return
.Llabel324:
.Llabel322:
	ret                                                 # SrcLine 498
	.size	kill_handler, .-kill_handler
.Lfunc_end25:


	.align	16
	.globl	scanEnvRoots
	.type	scanEnvRoots,@function
scanEnvRoots:                                               # @scanEnvRoots
.Lfunc_begin26:
.Llabel329:
.LBB26_0:                                                   # %entry
.Llabel330:
	pushl	%esi                                        # SrcLine 509
.Llabel327:
	subl	$8, %esp                                    # SrcLine 509
.Llabel328:
	movl	$0, 4(%esp)                                 # SrcLine 509
	jmp	.LBB26_8
.LBB26_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
.Llabel331:
	movl	4(%esp), %eax                               # SrcLine 512
	cmpl	$0, rdTable(,%eax,4)                        # SrcLine 512
	je	.LBB26_3
.LBB26_2:                                                   # %bb1
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
	movl	4(%esp), %esi                               # SrcLine 512
	movl	rdTable(,%esi,4), %eax                      # SrcLine 512
	movl	%eax, (%esp)                                # SrcLine 512
	call	copy                                        # SrcLine 512
	movl	%eax, rdTable(,%esi,4)                      # SrcLine 512
.LBB26_3:                                                   # %bb2
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
.Llabel332:
	movl	4(%esp), %eax                               # SrcLine 513
	cmpl	$0, wrTable(,%eax,4)                        # SrcLine 513
	je	.LBB26_5
.LBB26_4:                                                   # %bb3
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
	movl	4(%esp), %esi                               # SrcLine 513
	movl	wrTable(,%esi,4), %eax                      # SrcLine 513
	movl	%eax, (%esp)                                # SrcLine 513
	call	copy                                        # SrcLine 513
	movl	%eax, wrTable(,%esi,4)                      # SrcLine 513
.LBB26_5:                                                   # %bb4
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
.Llabel333:
	movl	4(%esp), %eax                               # SrcLine 514
	cmpl	$0, sockTable(,%eax,4)                      # SrcLine 514
	je	.LBB26_7
.LBB26_6:                                                   # %bb5
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
	movl	4(%esp), %esi                               # SrcLine 514
	movl	sockTable(,%esi,4), %eax                    # SrcLine 514
	movl	%eax, (%esp)                                # SrcLine 514
	call	copy                                        # SrcLine 514
	movl	%eax, sockTable(,%esi,4)                    # SrcLine 514
.LBB26_7:                                                   # %bb6
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
.Llabel334:
	incl	4(%esp)                                     # SrcLine 515
.Llabel335:
	movl	$envmut, (%esp)                             # SrcLine 516
	call	pthread_mutex_unlock                        # SrcLine 516
.LBB26_8:                                                   # %bb6
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel336:
	movl	$envmut, (%esp)                             # SrcLine 517
	call	pthread_mutex_lock                          # SrcLine 517
.LBB26_9:                                                   # %bb7
                                                            # Loop Depth 1
                                                            # Loop Header is BB26_8
                                                            # Inner Loop
.Llabel337:
	movl	maxDesc, %eax                               # SrcLine 511
	incl	%eax                                        # SrcLine 511
	cmpl	4(%esp), %eax                               # SrcLine 511
	jg	.LBB26_1                                    # SrcLine 511
.LBB26_10:                                                  # %bb8
.Llabel338:
	movl	$envmut, (%esp)                             # SrcLine 519
	call	pthread_mutex_unlock                        # SrcLine 519
.LBB26_11:                                                  # %return
.Llabel339:
.Llabel326:
	addl	$8, %esp                                    # SrcLine 520
	popl	%esi                                        # SrcLine 520
	ret                                                 # SrcLine 520
	.size	scanEnvRoots, .-scanEnvRoots
.Lfunc_end26:


	.align	16
	.globl	eventLoop
	.type	eventLoop,@function
eventLoop:                                                  # @eventLoop
.Lfunc_begin27:
.Llabel346:
.LBB27_0:                                                   # %entry
	pushl	%ebp
.Llabel341:
	pushl	%ebx
.Llabel342:
	pushl	%edi
.Llabel343:
	pushl	%esi
.Llabel344:
	subl	$508, %esp
.Llabel345:
	movl	528(%esp), %eax
	movl	%eax, 504(%esp)
.Llabel347:
	movl	%eax, 36(%esp)                              # SrcLine 528
.Llabel348:
	movl	current_key, %ecx                           # SrcLine 529
	movl	%eax, 4(%esp)                               # SrcLine 529
	movl	%ecx, (%esp)                                # SrcLine 529
	call	pthread_setspecific                         # SrcLine 529
.Llabel349:
	movl	36(%esp), %eax                              # SrcLine 531
	movl	8(%eax), %eax                               # SrcLine 531
	movl	%eax, 40(%esp)                              # SrcLine 531
.Llabel350:
	movl	36(%esp), %eax                              # SrcLine 532
	movl	12(%eax), %eax                              # SrcLine 532
	leal	40(%esp), %ecx
	movl	%ecx, 8(%esp)                               # SrcLine 532
	movl	%eax, (%esp)                                # SrcLine 532
	movl	$2, 4(%esp)                                 # SrcLine 532
	call	pthread_setschedparam                       # SrcLine 532
	leal	48(%esp), %esi
.Llabel351:
	movl	%esi, (%esp)                                # SrcLine 535
	call	sigemptyset                                 # SrcLine 535
.Llabel352:
	movl	%esi, (%esp)                                # SrcLine 536
	movl	$10, 4(%esp)                                # SrcLine 536
	call	sigaddset                                   # SrcLine 536
.Llabel353:
	movl	%esi, 4(%esp)                               # SrcLine 537
	movl	$0, 8(%esp)                                 # SrcLine 537
	movl	$1, (%esp)                                  # SrcLine 537
	call	pthread_sigmask                             # SrcLine 537
.Llabel354:
	movl	$envmut, (%esp)                             # SrcLine 539
	call	pthread_mutex_lock                          # SrcLine 539
	.align	16
.LBB27_1:                                                   # %bb
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            #   Child Loop BB27_24 Depth 2
                                                            #     Child Loop BB27_10 Depth 3
	movl	$32, %eax
	movl	%eax, %ecx
	leal	176(%esp), %ebx
	movl	%ebx, %edi
	movl	$readUsed, %edx
	movl	%edx, %esi
.Llabel355:
	rep;movsl                                           # SrcLine 543
	movl	%eax, %ecx
	leal	304(%esp), %ebp
	movl	%ebp, %edi
	movl	$writeUsed, %eax
	movl	%eax, %esi
.Llabel356:
	rep;movsl                                           # SrcLine 544
.Llabel357:
	movl	$envmut, (%esp)                             # SrcLine 545
	call	pthread_mutex_unlock                        # SrcLine 545
.Llabel358:
	movl	maxDesc, %eax                               # SrcLine 546
	movl	%ebp, 8(%esp)                               # SrcLine 546
	movl	%ebx, 4(%esp)                               # SrcLine 546
	incl	%eax                                        # SrcLine 546
	movl	%eax, (%esp)                                # SrcLine 546
	movl	$0, 16(%esp)                                # SrcLine 546
	movl	$0, 12(%esp)                                # SrcLine 546
	call	select                                      # SrcLine 546
	movl	%eax, 436(%esp)                             # SrcLine 546
.Llabel359:
	movl	$envmut, (%esp)                             # SrcLine 547
	call	pthread_mutex_lock                          # SrcLine 547
.Llabel360:
	cmpl	$0, 436(%esp)                               # SrcLine 548
	js	.LBB27_1                                    # SrcLine 548
.LBB27_2:                                                   # %bb5
                                                            # Loop Depth 1
                                                            # Loop Header is BB27_1
.Llabel361:
	movl	$0, 4(%esp)                                 # SrcLine 549
	movl	$evMsg+8, (%esp)                            # SrcLine 549
	call	gettimeofday                                # SrcLine 549
.Llabel362:
	movl	$0, 432(%esp)                               # SrcLine 550
	jmp	.LBB27_24
.LBB27_3:                                                   # %bb6
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel363:
	movl	432(%esp), %eax                             # SrcLine 551
	movl	%eax, %ecx                                  # SrcLine 551
	andl	$31, %ecx                                   # SrcLine 551
	andl	$4294967264, %eax                           # SrcLine 551
	shrl	$3, %eax                                    # SrcLine 551
	#APP
	btl %ecx,176(%esp,%eax) ; setcb %al
	#NO_APP
	movb	%al, 443(%esp)                              # SrcLine 551
	movb	%al, 23(%esp)                               # SrcLine 551
	testb	%al, %al                                    # SrcLine 551
	je	.LBB27_16
.LBB27_4:                                                   # %bb7
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel364:
	movl	432(%esp), %eax                             # SrcLine 552
	cmpl	$0, rdTable(,%eax,4)                        # SrcLine 552
	je	.LBB27_8                                    # SrcLine 552
.LBB27_5:                                                   # %bb8
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel365:
	movl	432(%esp), %eax                             # SrcLine 553
	movl	%eax, (%esp)                                # SrcLine 553
	call	read_descr                                  # SrcLine 553
	movl	%eax, 444(%esp)                             # SrcLine 553
	testl	%eax, %eax                                  # SrcLine 553
	jne	.LBB27_25
.LBB27_6:                                                   # %bb10
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel366:
	movl	432(%esp), %eax                             # SrcLine 557
	cmpl	$0, sockTable(,%eax,4)                      # SrcLine 557
	je	.LBB27_16
.LBB27_7:                                                   # %bb11
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel367:
	movl	432(%esp), %eax                             # SrcLine 558
	movl	sockTable(,%eax,4), %eax                    # SrcLine 558
	movl	24(%eax), %eax                              # SrcLine 558
	movl	4(%eax), %eax                               # SrcLine 558
	movl	%eax, 448(%esp)                             # SrcLine 558
.Llabel368:
	movl	%eax, (%esp)                                # SrcLine 559
	movl	$0, 4(%esp)                                 # SrcLine 559
	call	*4(%eax)                                    # SrcLine 559
.Llabel369:
	movl	432(%esp), %eax                             # SrcLine 560
	movl	%eax, (%esp)                                # SrcLine 560
	call	close                                       # SrcLine 560
.Llabel370:
	movl	432(%esp), %eax                             # SrcLine 561
	movl	$0, rdTable(,%eax,4)                        # SrcLine 561
	movl	432(%esp), %eax                             # SrcLine 561
	movl	%eax, %ecx                                  # SrcLine 561
	andl	$31, %ecx                                   # SrcLine 561
	andl	$4294967264, %eax                           # SrcLine 561
	shrl	$3, %eax                                    # SrcLine 561
	#APP
	btrl %ecx,readUsed(%eax)
	#NO_APP
.Llabel371:
	movl	432(%esp), %eax                             # SrcLine 562
	movl	$0, sockTable(,%eax,4)                      # SrcLine 562
	jmp	.LBB27_16
.LBB27_8:                                                   # %bb13
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel372:
	movl	432(%esp), %eax                             # SrcLine 564
	cmpl	$0, sockTable(,%eax,4)                      # SrcLine 564
	je	.LBB27_16
.LBB27_9:                                                   # %bb14
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel373:
	movl	$16, 452(%esp)                              # SrcLine 565
	leal	456(%esp), %eax
.Llabel374:
	movl	%eax, 24(%esp)                              # SrcLine 567
	movl	432(%esp), %ecx                             # SrcLine 567
	leal	452(%esp), %esi
	movl	%esi, 8(%esp)                               # SrcLine 567
	movl	%eax, 4(%esp)                               # SrcLine 567
	movl	%ecx, (%esp)                                # SrcLine 567
	call	accept                                      # SrcLine 567
	movl	%eax, 476(%esp)                             # SrcLine 567
.Llabel375:
	movl	%eax, (%esp)                                # SrcLine 568
	movl	$2048, 8(%esp)                              # SrcLine 568
	movl	$4, 4(%esp)                                 # SrcLine 568
	call	fcntl                                       # SrcLine 568
	.align	16
.LBB27_10:                                                  # %bb17
                                                            # Loop Depth 3
                                                            # Loop Header
                                                            # Inner Loop
                                                            #   Inside Loop BB27_24 Depth 2
                                                            # Inside Loop BB27_1 Depth 1
.Llabel376:
	movl	hp, %eax                                    # SrcLine 569
	movl	476(%esp), %ecx                             # SrcLine 569
	movl	%eax, sockTable(,%ecx,4)                    # SrcLine 569
	movl	lim, %eax                                   # SrcLine 569
	movl	%eax, 484(%esp)                             # SrcLine 569
	movl	476(%esp), %eax                             # SrcLine 569
	movl	sockTable(,%eax,4), %ecx                    # SrcLine 569
	addl	$28, %ecx                                   # SrcLine 569
	movl	%ecx, 480(%esp)                             # SrcLine 569
	movl	476(%esp), %eax                             # SrcLine 569
	movl	sockTable(,%eax,4), %edx                    # SrcLine 569
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 569
	cmpl	%edx, %eax                                  # SrcLine 569
	jne	.LBB27_10                                   # SrcLine 569
.LBB27_11:                                                  # %bb21
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
	movl	484(%esp), %eax                             # SrcLine 569
	cmpl	%eax, 480(%esp)                             # SrcLine 569
	jb	.LBB27_15
.LBB27_12:                                                  # %bb22
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
	movl	484(%esp), %eax                             # SrcLine 569
	movl	476(%esp), %esi                             # SrcLine 569
	cmpl	%eax, sockTable(,%esi,4)                    # SrcLine 569
	jb	.LBB27_26
.LBB27_13:                                                  # %bb24
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
	movl	$0, 28(%esp)                                # SrcLine 569
.LBB27_14:                                                  # %bb25
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
	movl	28(%esp), %eax                              # SrcLine 569
	movl	%eax, 4(%esp)                               # SrcLine 569
	movl	$7, (%esp)                                  # SrcLine 569
	call	force                                       # SrcLine 569
	movl	%eax, sockTable(,%esi,4)                    # SrcLine 569
.LBB27_15:                                                  # %bb26
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel377:
	movl	476(%esp), %eax                             # SrcLine 570
	movl	sockTable(,%eax,4), %eax                    # SrcLine 570
	movl	432(%esp), %ecx                             # SrcLine 570
	movl	sockTable(,%ecx,4), %ecx                    # SrcLine 570
	movl	20(%ecx), %ecx                              # SrcLine 570
	movl	%ecx, 20(%eax)                              # SrcLine 570
.Llabel378:
	movl	476(%esp), %eax                             # SrcLine 571
	movl	sockTable(,%eax,4), %eax                    # SrcLine 571
	movl	468(%esp), %ecx                             # SrcLine 571
	movl	%ecx, 16(%eax)                              # SrcLine 571
	movl	464(%esp), %ecx                             # SrcLine 571
	movl	%ecx, 12(%eax)                              # SrcLine 571
	movl	460(%esp), %ecx                             # SrcLine 571
	movl	%ecx, 8(%eax)                               # SrcLine 571
	movl	456(%esp), %ecx                             # SrcLine 571
	movl	%ecx, 4(%eax)                               # SrcLine 571
.Llabel379:
	movl	476(%esp), %eax                             # SrcLine 572
	movl	maxDesc, %ecx                               # SrcLine 572
	cmpl	%eax, %ecx                                  # SrcLine 572
	cmovge	%ecx, %eax                                  # SrcLine 572
	movl	%eax, maxDesc                               # SrcLine 572
.Llabel380:
	movl	476(%esp), %eax                             # SrcLine 573
	movl	%eax, (%esp)                                # SrcLine 573
	call	setupConnection                             # SrcLine 573
.LBB27_16:                                                  # %bb29
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel381:
	movl	432(%esp), %eax                             # SrcLine 576
	movl	%eax, %ecx                                  # SrcLine 576
	andl	$31, %ecx                                   # SrcLine 576
	andl	$4294967264, %eax                           # SrcLine 576
	shrl	$3, %eax                                    # SrcLine 576
	#APP
	btl %ecx,304(%esp,%eax) ; setcb %al
	#NO_APP
	movb	%al, 491(%esp)                              # SrcLine 576
	movb	%al, 35(%esp)                               # SrcLine 576
	testb	%al, %al                                    # SrcLine 576
	je	.LBB27_23
.LBB27_17:                                                  # %bb31
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel382:
	movl	432(%esp), %eax                             # SrcLine 577
	cmpl	$0, wrTable(,%eax,4)                        # SrcLine 577
	jne	.LBB27_27
.LBB27_18:                                                  # %bb33
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel383:
	movl	432(%esp), %eax                             # SrcLine 579
	cmpl	$0, sockTable(,%eax,4)                      # SrcLine 579
	je	.LBB27_23
.LBB27_19:                                                  # %bb34
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel384:
	movl	$4, 496(%esp)                               # SrcLine 581
.Llabel385:
	movl	432(%esp), %eax                             # SrcLine 582
	movl	%eax, %ecx                                  # SrcLine 582
	andl	$31, %ecx                                   # SrcLine 582
	andl	$4294967264, %eax                           # SrcLine 582
	shrl	$3, %eax                                    # SrcLine 582
	#APP
	btrl %ecx,writeUsed(%eax)
	#NO_APP
.Llabel386:
	movl	432(%esp), %eax                             # SrcLine 583
	leal	496(%esp), %edx
	movl	%edx, 16(%esp)                              # SrcLine 583
	leal	492(%esp), %edx
	movl	%edx, 12(%esp)                              # SrcLine 583
	movl	%eax, (%esp)                                # SrcLine 583
	movl	$4, 8(%esp)                                 # SrcLine 583
	movl	$1, 4(%esp)                                 # SrcLine 583
	call	getsockopt                                  # SrcLine 583
	testl	%eax, %eax                                  # SrcLine 583
	jns	.LBB27_21
.LBB27_20:                                                  # %bb36
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel387:
	movl	$.L.str1036, (%esp)                         # SrcLine 584
	call	perror                                      # SrcLine 584
.LBB27_21:                                                  # %bb37
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel388:
	cmpl	$0, 492(%esp)                               # SrcLine 585
	jne	.LBB27_28
.LBB27_22:                                                  # %bb39
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel389:
	movl	432(%esp), %eax                             # SrcLine 588
	movl	%eax, (%esp)                                # SrcLine 588
	call	setupConnection                             # SrcLine 588
.LBB27_23:                                                  # %bb40
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel390:
	incl	432(%esp)                                   # SrcLine 550
.LBB27_24:                                                  # %bb41
                                                            # Loop Depth 2
                                                            # Loop Header
                                                            #     Child Loop BB27_10 Depth 3
                                                            # Inside Loop BB27_1 Depth 1
	movl	maxDesc, %eax                               # SrcLine 550
	incl	%eax                                        # SrcLine 550
	cmpl	432(%esp), %eax                             # SrcLine 550
	jg	.LBB27_3
	jmp	.LBB27_1
.LBB27_25:                                                  # %bb9
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel391:
	movl	432(%esp), %eax                             # SrcLine 555
	movl	rdTable(,%eax,4), %eax                      # SrcLine 555
	movl	444(%esp), %ecx                             # SrcLine 555
	movl	%ecx, 4(%esp)                               # SrcLine 555
	movl	%eax, (%esp)                                # SrcLine 555
	movl	$0, 12(%esp)                                # SrcLine 555
	movl	$0, 8(%esp)                                 # SrcLine 555
	call	*4(%eax)                                    # SrcLine 555
	jmp	.LBB27_16
.LBB27_26:                                                  # %bb23
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel392:
	movl	476(%esp), %eax                             # SrcLine 569
	movl	sockTable(,%eax,4), %eax                    # SrcLine 569
	movl	%eax, 28(%esp)                              # SrcLine 569
	jmp	.LBB27_14
.LBB27_27:                                                  # %bb32
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel393:
	movl	432(%esp), %eax                             # SrcLine 578
	movl	wrTable(,%eax,4), %eax                      # SrcLine 578
	movl	%eax, (%esp)                                # SrcLine 578
	movl	$0, 8(%esp)                                 # SrcLine 578
	movl	$0, 4(%esp)                                 # SrcLine 578
	call	*4(%eax)                                    # SrcLine 578
	jmp	.LBB27_23
.LBB27_28:                                                  # %bb38
                                                            # Loop Depth 2
                                                            # Loop Header is BB27_24
                                                            # Inside Loop BB27_1 Depth 1
.Llabel394:
	movl	432(%esp), %eax                             # SrcLine 586
	movl	%eax, (%esp)                                # SrcLine 586
	movl	$.L.str1037, 4(%esp)                        # SrcLine 586
	call	netError                                    # SrcLine 586
	jmp	.LBB27_23
	.size	eventLoop, .-eventLoop
.Lfunc_end27:


	.align	16
	.globl	posix_POSIX
	.type	posix_POSIX,@function
posix_POSIX:                                                # @posix_POSIX
.Lfunc_begin28:
.Llabel400:
.LBB28_0:                                                   # %entry
	pushl	%edi
.Llabel397:
	pushl	%esi
.Llabel398:
	subl	$220, %esp
.Llabel399:
	movl	232(%esp), %eax
	movl	%eax, 216(%esp)
	movl	236(%esp), %eax
	movl	%eax, 212(%esp)
.Llabel401:
	movl	env, %eax                                   # SrcLine 601
	cmpl	$0, 8(%eax)                                 # SrcLine 601
	jne	.LBB28_11
.LBB28_1:                                                   # %bb
.Llabel402:
	movl	$glob_mutexattr, 4(%esp)                    # SrcLine 602
	movl	$envmut, (%esp)                             # SrcLine 602
	call	pthread_mutex_init                          # SrcLine 602
	xorl	%eax, %eax
	movl	$32, %ecx
	movl	$readUsed, %edi
.Llabel403:
	#APP
	cld; rep; stosl
	#NO_APP
	movl	%ecx, 184(%esp)                             # SrcLine 604
	movl	%edi, 188(%esp)                             # SrcLine 604
	movl	$32, %ecx
	movl	$writeUsed, %edi
.Llabel404:
	#APP
	cld; rep; stosl
	#NO_APP
	movl	%ecx, 192(%esp)                             # SrcLine 605
	movl	%edi, 196(%esp)                             # SrcLine 605
.Llabel405:
	movl	$0, 156(%esp)                               # SrcLine 608
.Llabel406:
	leal	28(%esp), %eax                              # SrcLine 609
	movl	%eax, (%esp)                                # SrcLine 609
	call	sigemptyset                                 # SrcLine 609
.Llabel407:
	movl	$kill_handler, 24(%esp)                     # SrcLine 610
	leal	24(%esp), %eax
.Llabel408:
	movl	%eax, 4(%esp)                               # SrcLine 611
	movl	$0, 8(%esp)                                 # SrcLine 611
	movl	$10, (%esp)                                 # SrcLine 611
	call	sigaction                                   # SrcLine 611
.Llabel409:
	call	getArgc                                     # SrcLine 613
	movl	%eax, 168(%esp)                             # SrcLine 613
.Llabel410:
	call	getArgv                                     # SrcLine 614
	movl	%eax, 172(%esp)                             # SrcLine 614
	.align	16
.LBB28_2:                                                   # %bb7
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
.Llabel411:
	movl	hp, %eax                                    # SrcLine 616
	movl	%eax, 176(%esp)                             # SrcLine 616
	movl	lim, %eax                                   # SrcLine 616
	movl	%eax, 204(%esp)                             # SrcLine 616
	movl	168(%esp), %eax                             # SrcLine 616
	movl	176(%esp), %ecx                             # SrcLine 616
	leal	8(%ecx,%eax,4), %ecx                        # SrcLine 616
	movl	%ecx, 200(%esp)                             # SrcLine 616
	movl	176(%esp), %edx                             # SrcLine 616
	movl	%edx, %eax
	lock
	cmpxchgl	%ecx, hp                            # SrcLine 616
	cmpl	%edx, %eax                                  # SrcLine 616
	jne	.LBB28_2                                    # SrcLine 616
.LBB28_3:                                                   # %bb11
	movl	204(%esp), %eax                             # SrcLine 616
	cmpl	%eax, 200(%esp)                             # SrcLine 616
	jb	.LBB28_7
.LBB28_4:                                                   # %bb12
	movl	204(%esp), %eax                             # SrcLine 616
	cmpl	%eax, 176(%esp)                             # SrcLine 616
	jb	.LBB28_13
.LBB28_5:                                                   # %bb14
	movl	$0, 16(%esp)                                # SrcLine 616
.LBB28_6:                                                   # %bb15
	movl	168(%esp), %eax                             # SrcLine 616
	movl	16(%esp), %ecx                              # SrcLine 616
	movl	%ecx, 4(%esp)                               # SrcLine 616
	addl	$2, %eax                                    # SrcLine 616
	movl	%eax, (%esp)                                # SrcLine 616
	call	force                                       # SrcLine 616
	movl	%eax, 176(%esp)                             # SrcLine 616
.LBB28_7:                                                   # %bb16
.Llabel412:
	movl	176(%esp), %eax                             # SrcLine 617
	movl	$__GC__Array0, (%eax)                       # SrcLine 617
.Llabel413:
	movl	168(%esp), %eax                             # SrcLine 618
	movl	176(%esp), %ecx                             # SrcLine 618
	movl	%eax, 4(%ecx)                               # SrcLine 618
.Llabel414:
	movl	$0, 180(%esp)                               # SrcLine 620
	jmp	.LBB28_9
.LBB28_8:                                                   # %bb17
                                                            # Loop Depth 1
                                                            # Loop Header is BB28_9
                                                            # Inner Loop
.Llabel415:
	movl	180(%esp), %esi                             # SrcLine 621
	movl	172(%esp), %eax                             # SrcLine 621
	movl	(%eax,%esi,4), %eax                         # SrcLine 621
	movl	%eax, (%esp)                                # SrcLine 621
	call	getStr                                      # SrcLine 621
	movl	176(%esp), %ecx                             # SrcLine 621
	movl	%eax, 8(%ecx,%esi,4)                        # SrcLine 621
.Llabel416:
	incl	180(%esp)                                   # SrcLine 620
.LBB28_9:                                                   # %bb18
                                                            # Loop Depth 1
                                                            # Loop Header
                                                            # Inner Loop
	movl	168(%esp), %eax                             # SrcLine 620
	cmpl	%eax, 180(%esp)                             # SrcLine 620
	jl	.LBB28_8                                    # SrcLine 620
.LBB28_10:                                                  # %bb19
.Llabel417:
	movl	176(%esp), %eax                             # SrcLine 622
	movl	env, %ecx                                   # SrcLine 622
	movl	%eax, 8(%ecx)                               # SrcLine 622
.Llabel418:
	movl	$2048, 8(%esp)                              # SrcLine 624
	movl	$4, 4(%esp)                                 # SrcLine 624
	movl	$0, (%esp)                                  # SrcLine 624
	call	fcntl                                       # SrcLine 624
.Llabel419:
	movl	$2048, 8(%esp)                              # SrcLine 625
	movl	$4, 4(%esp)                                 # SrcLine 625
	movl	$1, (%esp)                                  # SrcLine 625
	call	fcntl                                       # SrcLine 625
.Llabel420:
	movl	$0, 4(%esp)                                 # SrcLine 627
	movl	$evMsg+8, (%esp)                            # SrcLine 627
	call	gettimeofday                                # SrcLine 627
.Llabel421:
	movl	evMsg+8, %eax                               # SrcLine 628
	movl	%eax, startTime+4                           # SrcLine 628
.Llabel422:
	movl	evMsg+12, %eax                              # SrcLine 629
	movl	%eax, startTime+8                           # SrcLine 629
.Llabel423:
	movl	$scanner, (%esp)                            # SrcLine 631
	call	addRootScanner                              # SrcLine 631
.LBB28_11:                                                  # %bb20
.Llabel424:
	movl	env, %eax                                   # SrcLine 633
	movl	%eax, 20(%esp)                              # SrcLine 633
	movl	%eax, 208(%esp)                             # SrcLine 633
.LBB28_12:                                                  # %return
	movl	208(%esp), %eax
.Llabel396:
	addl	$220, %esp                                  # SrcLine 633
	popl	%esi                                        # SrcLine 633
	popl	%edi                                        # SrcLine 633
	ret                                                 # SrcLine 633
.LBB28_13:                                                  # %bb13
.Llabel425:
	movl	176(%esp), %eax                             # SrcLine 616
	movl	%eax, 16(%esp)                              # SrcLine 616
	jmp	.LBB28_6
	.size	posix_POSIX, .-posix_POSIX
.Lfunc_end28:


	.align	16
	.globl	startLoop
	.type	startLoop,@function
startLoop:                                                  # @startLoop
.Lfunc_begin29:
.Llabel429:
.LBB29_0:                                                   # %entry
.Llabel430:
	subl	$20, %esp                                   # SrcLine 640
.Llabel428:
	movl	$2, (%esp)                                  # SrcLine 640
	call	sched_get_priority_max                      # SrcLine 640
	movl	%eax, 4(%esp)                               # SrcLine 640
	movl	$0, 12(%esp)                                # SrcLine 640
	movl	$eventLoop, 8(%esp)                         # SrcLine 640
	movl	$evMsg, (%esp)                              # SrcLine 640
	call	newThread                                   # SrcLine 640
	movl	%eax, eventThread                           # SrcLine 640
.LBB29_1:                                                   # %return
.Llabel431:
.Llabel427:
	addl	$20, %esp                                   # SrcLine 641
	ret                                                 # SrcLine 641
	.size	startLoop, .-startLoop
.Lfunc_end29:
	.type	__GC__SockData,@object
	.data
	.globl __GC__SockData
	.align	4
__GC__SockData:                                             # @__GC__SockData
	.long	7                                           # 0x7
	.zero	4
	.long	5                                           # 0x5
	.zero	4
	.size	__GC__SockData, 16
	.type	evMsg,@object
	.globl evMsg
	.align	16
evMsg:                                                      # @evMsg
	.zero	4
	.zero	4
	.zero	8
	.long	2147483647                                  # 0x7FFFFFFF
	.zero	4
	.zero	4
	.size	evMsg, 28
	.type	eventThread,@object
	.bss
	.globl eventThread
	.align	4
eventThread:                                                # @eventThread
	.zero	4
	.size	eventThread, 4
	.type	maxDesc,@object
	.data
	.globl maxDesc
	.align	4
maxDesc:                                                    # @maxDesc
	.long	2                                           # 0x2
	.size	maxDesc, 4
	.type	__GC__DescClosable,@object
	.globl __GC__DescClosable
	.align	4
__GC__DescClosable:                                         # @__GC__DescClosable
	.long	3                                           # 0x3
	.zero	4
	.zero	4
	.size	__GC__DescClosable, 12
	.type	__GC__CloseMsg,@object
	.globl __GC__CloseMsg
	.align	4
__GC__CloseMsg:                                             # @__GC__CloseMsg
	.long	8                                           # 0x8
	.zero	4
	.zero	4
	.size	__GC__CloseMsg, 12
	.type	envmut,@object
	.section	.gnu.linkonce.b.envmut,"aw",@nobits
	.comm	envmut,24,16                                # @envmut
	.type	rdTable,@object
	.section	.gnu.linkonce.b.rdTable,"aw",@nobits
	.comm	rdTable,4096,32                             # @rdTable
	.type	readUsed,@object
	.section	.gnu.linkonce.b.readUsed,"aw",@nobits
	.comm	readUsed,128,32                             # @readUsed
	.type	wrTable,@object
	.section	.gnu.linkonce.b.wrTable,"aw",@nobits
	.comm	wrTable,4096,32                             # @wrTable
	.type	writeUsed,@object
	.section	.gnu.linkonce.b.writeUsed,"aw",@nobits
	.comm	writeUsed,128,32                            # @writeUsed
	.type	sockTable,@object
	.section	.gnu.linkonce.b.sockTable,"aw",@nobits
	.comm	sockTable,4096,32                           # @sockTable
	.type	envRootsDirty,@object
	.section	.gnu.linkonce.b.envRootsDirty,"aw",@nobits
	.comm	envRootsDirty,4,4                           # @envRootsDirty
	.type	rts,@object
	.section	.gnu.linkonce.b.rts,"aw",@nobits
	.comm	rts,24,16                                   # @rts
	.type	.L.str897,@object
	.section	.rodata.str1.16,"aMS",@progbits,1
.L.str897:                                                  # @.str897
	.asciz	"Name lookup error"
	.size	.L.str897, 18
	.type	.L.str909,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str909:                                                  # @.str909
	.asciz	"Connect failed"
	.size	.L.str909, 15
	.type	.L.str918,@object
.L.str918:                                                  # @.str918
	.asciz	"bind failed"
	.size	.L.str918, 12
	.type	stdin_cl,@object
	.data
	.globl stdin_cl
	.align	8
stdin_cl:                                                   # @stdin_cl
	.zero	4
	.long	close_fun
	.zero	4
	.size	stdin_cl, 12
	.type	stdout_cl,@object
	.globl stdout_cl
	.align	8
stdout_cl:                                                  # @stdout_cl
	.zero	4
	.long	close_fun
	.long	1                                           # 0x1
	.size	stdout_cl, 12
	.type	stdin_file,@object
	.globl stdin_file
	.align	8
stdin_file:                                                 # @stdin_file
	.zero	4
	.long	stdin_cl
	.long	seek_fun
	.size	stdin_file, 12
	.type	stdout_file,@object
	.globl stdout_file
	.align	8
stdout_file:                                                # @stdout_file
	.zero	4
	.long	stdout_cl
	.long	seek_fun
	.size	stdout_file, 12
	.type	stdin_rfile,@object
	.globl stdin_rfile
	.align	8
stdin_rfile:                                                # @stdin_rfile
	.zero	4
	.long	stdin_file
	.long	read_fun
	.long	installR_fun
	.size	stdin_rfile, 16
	.type	stdout_wfile,@object
	.globl stdout_wfile
	.align	8
stdout_wfile:                                               # @stdout_wfile
	.zero	4
	.long	stdout_file
	.long	write_fun
	.long	installW_fun
	.size	stdout_wfile, 16
	.type	tcp,@object
	.globl tcp
	.align	8
tcp:                                                        # @tcp
	.zero	4
	.long	connect_fun
	.long	listen_fun
	.size	tcp, 12
	.type	inet,@object
	.globl inet
	.align	8
inet:                                                       # @inet
	.zero	4
	.long	tcp
	.size	inet, 8
	.type	env_struct,@object
	.globl env_struct
	.align	32
env_struct:                                                 # @env_struct
	.zero	4
	.long	exit_fun
	.zero	4
	.long	stdin_rfile
	.long	stdout_wfile
	.long	openR_fun
	.long	openW_fun
	.long	startTime
	.long	inet
	.size	env_struct, 36
	.type	startTime,@object
	.section	.gnu.linkonce.b.startTime,"aw",@nobits
	.comm	startTime,12,8                              # @startTime
	.type	env,@object
	.data
	.globl env
	.align	4
env:                                                        # @env
	.long	env_struct
	.size	env, 4
	.type	scanner,@object
	.globl scanner
	.align	8
scanner:                                                    # @scanner
	.long	scanEnvRoots
	.zero	4
	.size	scanner, 8
	.type	.L.str1036,@object
	.section	.rodata.str1.16,"aMS",@progbits,1
.L.str1036:                                                 # @.str1036
	.asciz	"getsockopt failed"
	.size	.L.str1036, 18
	.type	.L.str1037,@object
.L.str1037:                                                 # @.str1037
	.asciz	"Connection failed"
	.size	.L.str1037, 18
	.type	glob_mutexattr,@object
	.section	.gnu.linkonce.b.glob_mutexattr,"aw",@nobits
	.comm	glob_mutexattr,4,8                          # @glob_mutexattr
	.text
.Ltext_end:
	.data
.Ldata_end:
	.text
.Lsection_end1:
	.section	.debug_frame,"",@progbits
.Ldebug_frame_common:
	.long	.Ldebug_frame_common_end-.Ldebug_frame_common_begin # Length of Common Information Entry
.Ldebug_frame_common_begin:
	.long	0xFFFFFFFF                                  # CIE Identifier Tag
	.byte	0x1                                         # CIE Version
	.asciz	""                                          # CIE Augmentation
	.uleb128	1                                   # CIE Code Alignment Factor
	.sleb128	-4                                  # CIE Data Alignment Factor
	.byte	0x8                                         # CIE RA Column
	.byte	0xC                                         # DW_CFA_def_cfa
	.uleb128	4                                   # Register
	.uleb128	4                                   # Offset
	.byte	0x88                                        # DW_CFA_offset + Reg (8)
	.uleb128	1                                   # Offset
	.align	4
.Ldebug_frame_common_end:

	.long	.Ldebug_frame_end1-.Ldebug_frame_begin1     # Length of Frame Information Entry
.Ldebug_frame_begin1:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin1                               # FDE initial location
	.long	.Lfunc_end1-.Lfunc_begin1                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel3-.Lfunc_begin1
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel4-.Llabel3
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end1:

	.long	.Ldebug_frame_end2-.Ldebug_frame_begin2     # Length of Frame Information Entry
.Ldebug_frame_begin2:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin2                               # FDE initial location
	.long	.Lfunc_end2-.Lfunc_begin2                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel13-.Lfunc_begin2
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel14-.Llabel13
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end2:

	.long	.Ldebug_frame_end3-.Ldebug_frame_begin3     # Length of Frame Information Entry
.Ldebug_frame_begin3:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin3                               # FDE initial location
	.long	.Lfunc_end3-.Lfunc_begin3                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel23-.Lfunc_begin3
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	32                                  # Offset
	.align	4
.Ldebug_frame_end3:

	.long	.Ldebug_frame_end4-.Ldebug_frame_begin4     # Length of Frame Information Entry
.Ldebug_frame_begin4:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin4                               # FDE initial location
	.long	.Lfunc_end4-.Lfunc_begin4                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel36-.Lfunc_begin4
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.align	4
.Ldebug_frame_end4:

	.long	.Ldebug_frame_end5-.Ldebug_frame_begin5     # Length of Frame Information Entry
.Ldebug_frame_begin5:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin5                               # FDE initial location
	.long	.Lfunc_end5-.Lfunc_begin5                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel46-.Lfunc_begin5
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	48                                  # Offset
	.align	4
.Ldebug_frame_end5:

	.long	.Ldebug_frame_end6-.Ldebug_frame_begin6     # Length of Frame Information Entry
.Ldebug_frame_begin6:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin6                               # FDE initial location
	.long	.Lfunc_end6-.Lfunc_begin6                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel58-.Lfunc_begin6
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.align	4
.Ldebug_frame_end6:

	.long	.Ldebug_frame_end7-.Ldebug_frame_begin7     # Length of Frame Information Entry
.Ldebug_frame_begin7:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin7                               # FDE initial location
	.long	.Lfunc_end7-.Lfunc_begin7                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel68-.Lfunc_begin7
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel69-.Llabel68
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	1096                                # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end7:

	.long	.Ldebug_frame_end8-.Ldebug_frame_begin8     # Length of Frame Information Entry
.Ldebug_frame_begin8:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin8                               # FDE initial location
	.long	.Lfunc_end8-.Lfunc_begin8                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel95-.Lfunc_begin8
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	24                                  # Offset
	.align	4
.Ldebug_frame_end8:

	.long	.Ldebug_frame_end9-.Ldebug_frame_begin9     # Length of Frame Information Entry
.Ldebug_frame_begin9:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin9                               # FDE initial location
	.long	.Lfunc_end9-.Lfunc_begin9                   # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel100-.Lfunc_begin9
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	48                                  # Offset
	.align	4
.Ldebug_frame_end9:

	.long	.Ldebug_frame_end10-.Ldebug_frame_begin10   # Length of Frame Information Entry
.Ldebug_frame_begin10:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin10                              # FDE initial location
	.long	.Lfunc_end10-.Lfunc_begin10                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel114-.Lfunc_begin10
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.align	4
.Ldebug_frame_end10:

	.long	.Ldebug_frame_end11-.Ldebug_frame_begin11   # Length of Frame Information Entry
.Ldebug_frame_begin11:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin11                              # FDE initial location
	.long	.Lfunc_end11-.Lfunc_begin11                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel125-.Lfunc_begin11
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel126-.Llabel125
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	1080                                # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end11:

	.long	.Ldebug_frame_end12-.Ldebug_frame_begin12   # Length of Frame Information Entry
.Ldebug_frame_begin12:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin12                              # FDE initial location
	.long	.Lfunc_end12-.Lfunc_begin12                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel141-.Lfunc_begin12
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	48                                  # Offset
	.align	4
.Ldebug_frame_end12:

	.long	.Ldebug_frame_end13-.Ldebug_frame_begin13   # Length of Frame Information Entry
.Ldebug_frame_begin13:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin13                              # FDE initial location
	.long	.Lfunc_end13-.Lfunc_begin13                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel156-.Lfunc_begin13
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.align	4
.Ldebug_frame_end13:

	.long	.Ldebug_frame_end14-.Ldebug_frame_begin14   # Length of Frame Information Entry
.Ldebug_frame_begin14:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin14                              # FDE initial location
	.long	.Lfunc_end14-.Lfunc_begin14                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel166-.Lfunc_begin14
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	24                                  # Offset
	.align	4
.Ldebug_frame_end14:

	.long	.Ldebug_frame_end15-.Ldebug_frame_begin15   # Length of Frame Information Entry
.Ldebug_frame_begin15:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin15                              # FDE initial location
	.long	.Lfunc_end15-.Lfunc_begin15                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel173-.Lfunc_begin15
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	1080                                # Offset
	.align	4
.Ldebug_frame_end15:

	.long	.Ldebug_frame_end16-.Ldebug_frame_begin16   # Length of Frame Information Entry
.Ldebug_frame_begin16:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin16                              # FDE initial location
	.long	.Lfunc_end16-.Lfunc_begin16                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel189-.Lfunc_begin16
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	56                                  # Offset
	.align	4
.Ldebug_frame_end16:

	.long	.Ldebug_frame_end17-.Ldebug_frame_begin17   # Length of Frame Information Entry
.Ldebug_frame_begin17:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin17                              # FDE initial location
	.long	.Lfunc_end17-.Lfunc_begin17                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel204-.Lfunc_begin17
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	56                                  # Offset
	.align	4
.Ldebug_frame_end17:

	.long	.Ldebug_frame_end18-.Ldebug_frame_begin18   # Length of Frame Information Entry
.Ldebug_frame_begin18:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin18                              # FDE initial location
	.long	.Lfunc_end18-.Lfunc_begin18                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel219-.Lfunc_begin18
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	72                                  # Offset
	.align	4
.Ldebug_frame_end18:

	.long	.Ldebug_frame_end19-.Ldebug_frame_begin19   # Length of Frame Information Entry
.Ldebug_frame_begin19:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin19                              # FDE initial location
	.long	.Lfunc_end19-.Lfunc_begin19                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel233-.Lfunc_begin19
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	48                                  # Offset
	.align	4
.Ldebug_frame_end19:

	.long	.Ldebug_frame_end20-.Ldebug_frame_begin20   # Length of Frame Information Entry
.Ldebug_frame_begin20:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin20                              # FDE initial location
	.long	.Lfunc_end20-.Lfunc_begin20                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel247-.Lfunc_begin20
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel248-.Llabel247
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	40                                  # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end20:

	.long	.Ldebug_frame_end21-.Ldebug_frame_begin21   # Length of Frame Information Entry
.Ldebug_frame_begin21:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin21                              # FDE initial location
	.long	.Lfunc_end21-.Lfunc_begin21                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel257-.Lfunc_begin21
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel258-.Llabel257
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	32                                  # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end21:

	.long	.Ldebug_frame_end22-.Ldebug_frame_begin22   # Length of Frame Information Entry
.Ldebug_frame_begin22:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin22                              # FDE initial location
	.long	.Lfunc_end22-.Lfunc_begin22                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel268-.Lfunc_begin22
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	1072                                # Offset
	.align	4
.Ldebug_frame_end22:

	.long	.Ldebug_frame_end23-.Ldebug_frame_begin23   # Length of Frame Information Entry
.Ldebug_frame_begin23:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin23                              # FDE initial location
	.long	.Lfunc_end23-.Lfunc_begin23                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel284-.Lfunc_begin23
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	80                                  # Offset
	.align	4
.Ldebug_frame_end23:

	.long	.Ldebug_frame_end24-.Ldebug_frame_begin24   # Length of Frame Information Entry
.Ldebug_frame_begin24:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin24                              # FDE initial location
	.long	.Lfunc_end24-.Lfunc_begin24                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel305-.Lfunc_begin24
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	72                                  # Offset
	.align	4
.Ldebug_frame_end24:

	.long	.Ldebug_frame_end25-.Ldebug_frame_begin25   # Length of Frame Information Entry
.Ldebug_frame_begin25:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin25                              # FDE initial location
	.long	.Lfunc_end25-.Lfunc_begin25                 # FDE address range
	.align	4
.Ldebug_frame_end25:

	.long	.Ldebug_frame_end26-.Ldebug_frame_begin26   # Length of Frame Information Entry
.Ldebug_frame_begin26:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin26                              # FDE initial location
	.long	.Lfunc_end26-.Lfunc_begin26                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel327-.Lfunc_begin26
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel328-.Llabel327
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	16                                  # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end26:

	.long	.Ldebug_frame_end27-.Ldebug_frame_begin27   # Length of Frame Information Entry
.Ldebug_frame_begin27:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin27                              # FDE initial location
	.long	.Lfunc_end27-.Lfunc_begin27                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel341-.Lfunc_begin27
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel342-.Llabel341
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	12                                  # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel343-.Llabel342
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	16                                  # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel344-.Llabel343
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	20                                  # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel345-.Llabel344
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	528                                 # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	5                                   # Offset
	.byte	0x87                                        # DW_CFA_offset + Reg (7)
	.uleb128	4                                   # Offset
	.byte	0x83                                        # DW_CFA_offset + Reg (3)
	.uleb128	3                                   # Offset
	.byte	0x85                                        # DW_CFA_offset + Reg (5)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end27:

	.long	.Ldebug_frame_end28-.Ldebug_frame_begin28   # Length of Frame Information Entry
.Ldebug_frame_begin28:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin28                              # FDE initial location
	.long	.Lfunc_end28-.Lfunc_begin28                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel397-.Lfunc_begin28
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	8                                   # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel398-.Llabel397
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	12                                  # Offset
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel399-.Llabel398
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	232                                 # Offset
	.byte	0x86                                        # DW_CFA_offset + Reg (6)
	.uleb128	3                                   # Offset
	.byte	0x87                                        # DW_CFA_offset + Reg (7)
	.uleb128	2                                   # Offset
	.align	4
.Ldebug_frame_end28:

	.long	.Ldebug_frame_end29-.Ldebug_frame_begin29   # Length of Frame Information Entry
.Ldebug_frame_begin29:
	.long	.Ldebug_frame_common                        # FDE CIE offset
	.long	.Lfunc_begin29                              # FDE initial location
	.long	.Lfunc_end29-.Lfunc_begin29                 # FDE address range
	.byte	0x4                                         # DW_CFA_advance_loc4
	.long	.Llabel428-.Lfunc_begin29
	.byte	0xE                                         # DW_CFA_def_cfa_offset
	.uleb128	24                                  # Offset
	.align	4
.Ldebug_frame_end29:

	.section	.debug_info,"",@progbits
.Linfo_begin1:
	.long	0x1F09                                      # Length of Compilation Unit Info
	.short	0x2                                         # DWARF version number
	.long	.Labbrev_begin                              # Offset Into Abbrev. Section
	.byte	0x4                                         # Address Size (in bytes)

	.uleb128	1                                   # Abbrev [1] 0xB:0x1EFE DW_TAG_compile_unit
	.long	.Lsection_line                              # DW_AT_stmt_list
	.asciz	"4.2.1 (Based on Apple Inc. build 5649) (LLVM build)" # DW_AT_producer
	.byte	0x1                                         # DW_AT_language
	.asciz	"POSIX.extern.c"                            # DW_AT_name
	.asciz	"/home/capitrane/thesis/timber/lib/"        # DW_AT_comp_dir

	.uleb128	2                                   # Abbrev [2] 0x77:0x7 DW_TAG_base_type
	.byte	0x5                                         # DW_AT_encoding
	.asciz	"int"                                       # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size

	.uleb128	3                                   # Abbrev [3] 0x7E:0x11 DW_TAG_typedef
	.long	0x77                                        # DW_AT_type
	.asciz	"__int32_t"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x29                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x8F:0xF DW_TAG_typedef
	.long	0x7E                                        # DW_AT_type
	.asciz	"__pid_t"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x8F                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x9E:0x11 DW_TAG_typedef
	.long	0x8F                                        # DW_AT_type
	.asciz	"__daddr_t"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x99                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xAF:0xF DW_TAG_typedef
	.long	0x9E                                        # DW_AT_type
	.asciz	"__key_t"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x9B                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xBE:0x13 DW_TAG_typedef
	.long	0xAF                                        # DW_AT_type
	.asciz	"__clockid_t"                               # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x9E                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xD1:0x11 DW_TAG_typedef
	.long	0xBE                                        # DW_AT_type
	.asciz	"__ssize_t"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xB4                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xE2:0x12 DW_TAG_typedef
	.long	0xD1                                        # DW_AT_type
	.asciz	"__intptr_t"                                # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xBD                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xF4:0xD DW_TAG_typedef
	.long	0xE2                                        # DW_AT_type
	.asciz	"pid_t"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x64                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x101:0xF DW_TAG_typedef
	.long	0xF4                                        # DW_AT_type
	.asciz	"ssize_t"                                   # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x6E                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x110:0xF DW_TAG_typedef
	.long	0x101                                       # DW_AT_type
	.asciz	"daddr_t"                                   # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x74                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x11F:0xD DW_TAG_typedef
	.long	0x110                                       # DW_AT_type
	.asciz	"key_t"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x7B                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x12C:0x11 DW_TAG_typedef
	.long	0x11F                                       # DW_AT_type
	.asciz	"clockid_t"                                 # DW_AT_name
	.byte	0xD                                         # DW_AT_decl_file
	.byte	0x5C                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x13D:0xF DW_TAG_typedef
	.long	0x12C                                       # DW_AT_type
	.asciz	"int32_t"                                   # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0xC5                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x14C:0x12 DW_TAG_typedef
	.long	0x13D                                       # DW_AT_type
	.asciz	"register_t"                                # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0xCE                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15E:0x16 DW_TAG_typedef
	.long	0x14C                                       # DW_AT_type
	.asciz	"__sig_atomic_t"                            # DW_AT_name
	.byte	0xB                                         # DW_AT_decl_file
	.byte	0x18                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x174:0x16 DW_TAG_typedef
	.long	0x15E                                       # DW_AT_type
	.asciz	"pthread_once_t"                            # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x90                                        # DW_AT_decl_line

	.uleb128	4                                   # Abbrev [4] 0x18A:0x8 DW_TAG_volatile_type
	.long	0x174                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x192:0x1A DW_TAG_typedef
	.long	0x18A                                       # DW_AT_type
	.asciz	"pthread_spinlock_t"                        # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0xC9                                        # DW_AT_decl_line

	.uleb128	5                                   # Abbrev [5] 0x1AC:0x8 DW_TAG_const_type
	.long	0x192                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1B4:0x10 DW_TAG_typedef
	.long	0x1AC                                       # DW_AT_type
	.asciz	"intptr_t"                                  # DW_AT_name
	.byte	0x9                                         # DW_AT_decl_file
	.byte	0xF0                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1C4:0x15 DW_TAG_typedef
	.long	0x1B4                                       # DW_AT_type
	.asciz	"int_least32_t"                             # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x44                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1D9:0x14 DW_TAG_typedef
	.long	0x1C4                                       # DW_AT_type
	.asciz	"int_fast16_t"                              # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x61                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1ED:0x14 DW_TAG_typedef
	.long	0x1D9                                       # DW_AT_type
	.asciz	"int_fast32_t"                              # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x62                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x201:0xF DW_TAG_typedef
	.long	0x1ED                                       # DW_AT_type
	.asciz	"error_t"                                   # DW_AT_name
	.byte	0x7                                         # DW_AT_decl_file
	.byte	0x45                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x210:0x11 DW_TAG_typedef
	.long	0x201                                       # DW_AT_type
	.asciz	"ptrdiff_t"                                 # DW_AT_name
	.byte	0x6                                         # DW_AT_decl_file
	.byte	0x98                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x221:0x14 DW_TAG_typedef
	.long	0x210                                       # DW_AT_type
	.asciz	"sig_atomic_t"                              # DW_AT_name
	.byte	0x5                                         # DW_AT_decl_file
	.byte	0x29                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x235:0xE DW_TAG_typedef
	.long	0x221                                       # DW_AT_type
	.asciz	"greg_t"                                    # DW_AT_name
	.byte	0x4                                         # DW_AT_decl_file
	.byte	0x94                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x243:0xC DW_TAG_typedef
	.long	0x235                                       # DW_AT_type
	.asciz	"WORD"                                      # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x2D                                        # DW_AT_decl_line

	.uleb128	6                                   # Abbrev [6] 0x24F:0x3 DW_TAG_base_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x5                                         # DW_AT_encoding

	.uleb128	7                                   # Abbrev [7] 0x252:0x10 DW_TAG_array_type
	.long	0x262                                       # DW_AT_sibling
	.long	0x243                                       # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x25B:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x262:0x2C DW_TAG_variable
	.asciz	"__GC__SockData"                            # DW_AT_name
	.asciz	"__GC__SockData"                            # DW_AT_MIPS_linkage_name
	.long	0x252                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x46                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	__GC__SockData                              # DW_AT_location

	.uleb128	10                                  # Abbrev [10] 0x28E:0x8 DW_TAG_pointer_type
	.long	0x243                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0x296:0x8 DW_TAG_pointer_type
	.long	0x35D                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x29E:0xB DW_TAG_typedef
	.long	0x296                                       # DW_AT_type
	.asciz	"Msg"                                       # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x71                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0x2A9:0x10 DW_TAG_subroutine_type
	.long	0x2B9                                       # DW_AT_sibling
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0x2B3:0x5 DW_TAG_formal_parameter
	.long	0x29E                                       # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x2B9:0x8 DW_TAG_pointer_type
	.long	0x2A9                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	2                                   # Abbrev [2] 0x2C1:0xC DW_TAG_base_type
	.byte	0x5                                         # DW_AT_encoding
	.asciz	"long int"                                  # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size

	.uleb128	3                                   # Abbrev [3] 0x2CD:0xF DW_TAG_typedef
	.long	0x2C1                                       # DW_AT_type
	.asciz	"__off_t"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x8D                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x2DC:0x11 DW_TAG_typedef
	.long	0x2CD                                       # DW_AT_type
	.asciz	"__clock_t"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x91                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x2ED:0x10 DW_TAG_typedef
	.long	0x2DC                                       # DW_AT_type
	.asciz	"__time_t"                                  # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x95                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x2FD:0x15 DW_TAG_typedef
	.long	0x2ED                                       # DW_AT_type
	.asciz	"__suseconds_t"                             # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x97                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x312:0x34 DW_TAG_structure_type
	.long	0x346                                       # DW_AT_sibling
	.asciz	"timeval"                                   # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x14                                        # DW_AT_decl_file
	.byte	0x46                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x322:0x11 DW_TAG_member
	.asciz	"tv_sec"                                    # DW_AT_name
	.long	0x2ED                                       # DW_AT_type
	.byte	0x14                                        # DW_AT_decl_file
	.byte	0x47                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x333:0x12 DW_TAG_member
	.asciz	"tv_usec"                                   # DW_AT_name
	.long	0x2FD                                       # DW_AT_type
	.byte	0x14                                        # DW_AT_decl_file
	.byte	0x48                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	5                                   # Abbrev [5] 0x346:0x8 DW_TAG_const_type
	.long	0x312                                       # DW_AT_type
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x34E:0xF DW_TAG_typedef
	.long	0x346                                       # DW_AT_type
	.asciz	"AbsTime"                                   # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x65                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x35D:0x62 DW_TAG_structure_type
	.long	0x3BF                                       # DW_AT_sibling
	.asciz	"Msg"                                       # DW_AT_name
	.byte	0x1C                                        # DW_AT_byte_size
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x71                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x369:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x28E                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x9F                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x37A:0xF DW_TAG_member
	.asciz	"Code"                                      # DW_AT_name
	.long	0x2B9                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA0                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x389:0x13 DW_TAG_member
	.asciz	"baseline"                                  # DW_AT_name
	.long	0x34E                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA1                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x39C:0x13 DW_TAG_member
	.asciz	"deadline"                                  # DW_AT_name
	.long	0x34E                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA2                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	16                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x3AF:0xF DW_TAG_member
	.asciz	"next"                                      # DW_AT_name
	.long	0x29E                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA3                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	24                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x3BF:0x1A DW_TAG_variable
	.asciz	"evMsg"                                     # DW_AT_name
	.asciz	"evMsg"                                     # DW_AT_MIPS_linkage_name
	.long	0x35D                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x6C                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	evMsg                                       # DW_AT_location

	.uleb128	10                                  # Abbrev [10] 0x3D9:0x8 DW_TAG_pointer_type
	.long	0x612                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x3E1:0xE DW_TAG_typedef
	.long	0x3D9                                       # DW_AT_type
	.asciz	"Thread"                                    # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x6F                                        # DW_AT_decl_line

	.uleb128	2                                   # Abbrev [2] 0x3EF:0x15 DW_TAG_base_type
	.byte	0x7                                         # DW_AT_encoding
	.asciz	"long unsigned int"                         # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size

	.uleb128	3                                   # Abbrev [3] 0x404:0x10 DW_TAG_typedef
	.long	0x3EF                                       # DW_AT_type
	.asciz	"__u_long"                                  # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x22                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x414:0xF DW_TAG_typedef
	.long	0x404                                       # DW_AT_type
	.asciz	"__ino_t"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x89                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x423:0x10 DW_TAG_typedef
	.long	0x414                                       # DW_AT_type
	.asciz	"__rlim_t"                                  # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x92                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x433:0x14 DW_TAG_typedef
	.long	0x423                                       # DW_AT_type
	.asciz	"__fsblkcnt_t"                              # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xAD                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x447:0x14 DW_TAG_typedef
	.long	0x433                                       # DW_AT_type
	.asciz	"__fsfilcnt_t"                              # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xB1                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x45B:0xE DW_TAG_typedef
	.long	0x447                                       # DW_AT_type
	.asciz	"u_long"                                    # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x26                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x469:0xD DW_TAG_typedef
	.long	0x45B                                       # DW_AT_type
	.asciz	"ino_t"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x32                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x476:0xD DW_TAG_typedef
	.long	0x469                                       # DW_AT_type
	.asciz	"ulong"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x97                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x483:0x12 DW_TAG_typedef
	.long	0x476                                       # DW_AT_type
	.asciz	"fsblkcnt_t"                                # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0xEF                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x495:0x12 DW_TAG_typedef
	.long	0x483                                       # DW_AT_type
	.asciz	"fsfilcnt_t"                                # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0xF3                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x4A7:0x11 DW_TAG_typedef
	.long	0x495                                       # DW_AT_type
	.asciz	"pthread_t"                                 # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x32                                        # DW_AT_decl_line

	.uleb128	2                                   # Abbrev [2] 0x4B8:0x10 DW_TAG_base_type
	.byte	0x7                                         # DW_AT_encoding
	.asciz	"unsigned int"                              # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size

	.uleb128	2                                   # Abbrev [2] 0x4C8:0x1A DW_TAG_base_type
	.byte	0x7                                         # DW_AT_encoding
	.asciz	"long long unsigned int"                    # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size

	.uleb128	15                                  # Abbrev [15] 0x4E2:0x4 DW_TAG_pointer_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	16                                  # Abbrev [16] 0x4E6:0xB0 DW_TAG_structure_type
	.long	0x596                                       # DW_AT_sibling
	.byte	0x2C                                        # DW_AT_byte_size
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x76                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x4EE:0x11 DW_TAG_member
	.asciz	"__lock"                                    # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x77                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x4FF:0x12 DW_TAG_member
	.asciz	"__futex"                                   # DW_AT_name
	.long	0x4B8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x78                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x511:0x16 DW_TAG_member
	.asciz	"__total_seq"                               # DW_AT_name
	.long	0x4C8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x79                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x527:0x17 DW_TAG_member
	.asciz	"__wakeup_seq"                              # DW_AT_name
	.long	0x4C8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x7A                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	16                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x53E:0x16 DW_TAG_member
	.asciz	"__woken_seq"                               # DW_AT_name
	.long	0x4C8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x7B                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	24                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x554:0x12 DW_TAG_member
	.asciz	"__mutex"                                   # DW_AT_name
	.long	0x4E2                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x7C                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	32                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x566:0x15 DW_TAG_member
	.asciz	"__nwaiters"                                # DW_AT_name
	.long	0x4B8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x7D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	36                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x57B:0x1A DW_TAG_member
	.asciz	"__broadcast_seq"                           # DW_AT_name
	.long	0x4B8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x7E                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	40                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	2                                   # Abbrev [2] 0x596:0x8 DW_TAG_base_type
	.byte	0x6                                         # DW_AT_encoding
	.asciz	"char"                                      # DW_AT_name
	.byte	0x1                                         # DW_AT_byte_size

	.uleb128	7                                   # Abbrev [7] 0x59E:0x10 DW_TAG_array_type
	.long	0x5AE                                       # DW_AT_sibling
	.long	0x596                                       # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x5A7:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x2F                                        # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	2                                   # Abbrev [2] 0x5AE:0x11 DW_TAG_base_type
	.byte	0x5                                         # DW_AT_encoding
	.asciz	"long long int"                             # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size

	.uleb128	17                                  # Abbrev [17] 0x5BF:0x3D DW_TAG_union_type
	.long	0x5FC                                       # DW_AT_sibling
	.byte	0x30                                        # DW_AT_byte_size
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x74                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x5C7:0x11 DW_TAG_member
	.asciz	"__data"                                    # DW_AT_name
	.long	0x4E6                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x7F                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x5D8:0x11 DW_TAG_member
	.asciz	"__size"                                    # DW_AT_name
	.long	0x59E                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x80                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x5E9:0x12 DW_TAG_member
	.asciz	"__align"                                   # DW_AT_name
	.long	0x5AE                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x81                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0x5FC:0x16 DW_TAG_typedef
	.long	0x5BF                                       # DW_AT_type
	.asciz	"pthread_cond_t"                            # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x82                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x612:0x82 DW_TAG_structure_type
	.long	0x694                                       # DW_AT_sibling
	.asciz	"Thread"                                    # DW_AT_name
	.byte	0x48                                        # DW_AT_byte_size
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x6F                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x621:0xF DW_TAG_member
	.asciz	"next"                                      # DW_AT_name
	.long	0x3E1                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x74                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x630:0xE DW_TAG_member
	.asciz	"msg"                                       # DW_AT_name
	.long	0x29E                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x75                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x63E:0xF DW_TAG_member
	.asciz	"prio"                                      # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x76                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x64D:0xD DW_TAG_member
	.asciz	"id"                                        # DW_AT_name
	.long	0x4A7                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x77                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x65A:0x10 DW_TAG_member
	.asciz	"index"                                     # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x78                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	16                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x66A:0x12 DW_TAG_member
	.asciz	"trigger"                                   # DW_AT_name
	.long	0x5FC                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x79                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	20                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x67C:0x17 DW_TAG_member
	.asciz	"placeholders"                              # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x7A                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	68                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0x694:0xE DW_TAG_typedef
	.long	0x3D9                                       # DW_AT_type
	.asciz	"Thread"                                    # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x6F                                        # DW_AT_decl_line

	.uleb128	9                                   # Abbrev [9] 0x6A2:0x26 DW_TAG_variable
	.asciz	"eventThread"                               # DW_AT_name
	.asciz	"eventThread"                               # DW_AT_MIPS_linkage_name
	.long	0x694                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x6E                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	eventThread                                 # DW_AT_location

	.uleb128	9                                   # Abbrev [9] 0x6C8:0x1E DW_TAG_variable
	.asciz	"maxDesc"                                   # DW_AT_name
	.asciz	"maxDesc"                                   # DW_AT_MIPS_linkage_name
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x72                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	maxDesc                                     # DW_AT_location

	.uleb128	7                                   # Abbrev [7] 0x6E6:0x10 DW_TAG_array_type
	.long	0x6F6                                       # DW_AT_sibling
	.long	0x243                                       # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x6EF:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x6F6:0x34 DW_TAG_variable
	.asciz	"__GC__DescClosable"                        # DW_AT_name
	.asciz	"__GC__DescClosable"                        # DW_AT_MIPS_linkage_name
	.long	0x6E6                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x90                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	__GC__DescClosable                          # DW_AT_location

	.uleb128	7                                   # Abbrev [7] 0x72A:0x10 DW_TAG_array_type
	.long	0x73A                                       # DW_AT_sibling
	.long	0x243                                       # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x733:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x73A:0x2C DW_TAG_variable
	.asciz	"__GC__CloseMsg"                            # DW_AT_name
	.asciz	"__GC__CloseMsg"                            # DW_AT_MIPS_linkage_name
	.long	0x72A                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xA0                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	__GC__CloseMsg                              # DW_AT_location

	.uleb128	3                                   # Abbrev [3] 0x766:0xC DW_TAG_typedef
	.long	0x28E                                       # DW_AT_type
	.asciz	"ADDR"                                      # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x2E                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x772:0x3F DW_TAG_structure_type
	.long	0x7B1                                       # DW_AT_sibling
	.asciz	"Closable_POSIX"                            # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x5                                         # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x789:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x21                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x79A:0x16 DW_TAG_member
	.asciz	"close_POSIX"                               # DW_AT_name
	.long	0x7E4                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x22                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x7B1:0x8 DW_TAG_pointer_type
	.long	0x772                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x7B9:0x16 DW_TAG_typedef
	.long	0x7B1                                       # DW_AT_type
	.asciz	"Closable_POSIX"                            # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x6                                         # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0x7CF:0x15 DW_TAG_subroutine_type
	.long	0x7E4                                       # DW_AT_sibling
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0x7D9:0x5 DW_TAG_formal_parameter
	.long	0x7B9                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x7DE:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x7E4:0x8 DW_TAG_pointer_type
	.long	0x7CF                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x7EC:0x52 DW_TAG_structure_type
	.long	0x83E                                       # DW_AT_sibling
	.asciz	"DescClosable"                              # DW_AT_name
	.byte	0xC                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x8A                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x801:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x28E                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x8B                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x812:0x16 DW_TAG_member
	.asciz	"close_POSIX"                               # DW_AT_name
	.long	0x7E4                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x8C                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x828:0x15 DW_TAG_member
	.asciz	"descriptor"                                # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x8D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0x83E:0x21 DW_TAG_variable
	.asciz	"stdin_cl"                                  # DW_AT_name
	.asciz	"stdin_cl"                                  # DW_AT_MIPS_linkage_name
	.long	0x7EC                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1DF                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	stdin_cl                                    # DW_AT_location

	.uleb128	18                                  # Abbrev [18] 0x85F:0x23 DW_TAG_variable
	.asciz	"stdout_cl"                                 # DW_AT_name
	.asciz	"stdout_cl"                                 # DW_AT_MIPS_linkage_name
	.long	0x7EC                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1E0                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	stdout_cl                                   # DW_AT_location

	.uleb128	3                                   # Abbrev [3] 0x882:0x16 DW_TAG_typedef
	.long	0x7B1                                       # DW_AT_type
	.asciz	"Closable_POSIX"                            # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x6                                         # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0x898:0x8 DW_TAG_pointer_type
	.long	0x8D4                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x8A0:0x12 DW_TAG_typedef
	.long	0x898                                       # DW_AT_type
	.asciz	"File_POSIX"                                # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x8                                         # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0x8B2:0x1A DW_TAG_subroutine_type
	.long	0x8CC                                       # DW_AT_sibling
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0x8BC:0x5 DW_TAG_formal_parameter
	.long	0x8A0                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x8C1:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x8C6:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x8CC:0x8 DW_TAG_pointer_type
	.long	0x8B2                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x8D4:0x66 DW_TAG_structure_type
	.long	0x93A                                       # DW_AT_sibling
	.asciz	"File_POSIX"                                # DW_AT_name
	.byte	0xC                                         # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x7                                         # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x8E7:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x25                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x8F8:0x2C DW_TAG_member
	.asciz	"l_File_POSIX_Closable_POSIX_POSIX"         # DW_AT_name
	.long	0x882                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x26                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x924:0x15 DW_TAG_member
	.asciz	"seek_POSIX"                                # DW_AT_name
	.long	0x8CC                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x27                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0x93A:0x25 DW_TAG_variable
	.asciz	"stdin_file"                                # DW_AT_name
	.asciz	"stdin_file"                                # DW_AT_MIPS_linkage_name
	.long	0x8D4                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1E2                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	stdin_file                                  # DW_AT_location

	.uleb128	18                                  # Abbrev [18] 0x95F:0x27 DW_TAG_variable
	.asciz	"stdout_file"                               # DW_AT_name
	.asciz	"stdout_file"                               # DW_AT_MIPS_linkage_name
	.long	0x8D4                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1E3                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	stdout_file                                 # DW_AT_location

	.uleb128	3                                   # Abbrev [3] 0x986:0x12 DW_TAG_typedef
	.long	0x898                                       # DW_AT_type
	.asciz	"File_POSIX"                                # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x8                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x998:0x1F DW_TAG_structure_type
	.long	0x9B7                                       # DW_AT_sibling
	.asciz	"LIST"                                      # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x38                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x9A5:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x28E                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x80                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x9B7:0x8 DW_TAG_pointer_type
	.long	0x998                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x9BF:0xC DW_TAG_typedef
	.long	0x9B7                                       # DW_AT_type
	.asciz	"LIST"                                      # DW_AT_name
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x39                                        # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0x9CB:0x8 DW_TAG_pointer_type
	.long	0xA9D                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x9D3:0x13 DW_TAG_typedef
	.long	0x9CB                                       # DW_AT_type
	.asciz	"RFile_POSIX"                               # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xA                                         # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0x9E6:0x15 DW_TAG_subroutine_type
	.long	0x9FB                                       # DW_AT_sibling
	.long	0x9BF                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0x9F0:0x5 DW_TAG_formal_parameter
	.long	0x9D3                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x9F5:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x9FB:0x8 DW_TAG_pointer_type
	.long	0x9E6                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0xA03:0x8 DW_TAG_pointer_type
	.long	0xA3F                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xA0B:0xD DW_TAG_typedef
	.long	0xA03                                       # DW_AT_type
	.asciz	"CLOS3"                                     # DW_AT_name
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x34                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xA18:0x1F DW_TAG_subroutine_type
	.long	0xA37                                       # DW_AT_sibling
	.long	0x766                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xA22:0x5 DW_TAG_formal_parameter
	.long	0xA0B                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xA27:0x5 DW_TAG_formal_parameter
	.long	0x766                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xA2C:0x5 DW_TAG_formal_parameter
	.long	0x766                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xA31:0x5 DW_TAG_formal_parameter
	.long	0x766                                       # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xA37:0x8 DW_TAG_pointer_type
	.long	0xA18                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xA3F:0x2F DW_TAG_structure_type
	.long	0xA6E                                       # DW_AT_sibling
	.asciz	"CLOS3"                                     # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x33                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xA4D:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x74                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xA5E:0xF DW_TAG_member
	.asciz	"Code"                                      # DW_AT_name
	.long	0xA37                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x75                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0xA6E:0xD DW_TAG_typedef
	.long	0xA03                                       # DW_AT_type
	.asciz	"CLOS3"                                     # DW_AT_name
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x34                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xA7B:0x1A DW_TAG_subroutine_type
	.long	0xA95                                       # DW_AT_sibling
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xA85:0x5 DW_TAG_formal_parameter
	.long	0x9D3                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xA8A:0x5 DW_TAG_formal_parameter
	.long	0xA6E                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xA8F:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xA95:0x8 DW_TAG_pointer_type
	.long	0xA7B                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xA9D:0x7D DW_TAG_structure_type
	.long	0xB1A                                       # DW_AT_sibling
	.asciz	"RFile_POSIX"                               # DW_AT_name
	.byte	0x10                                        # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x9                                         # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xAB1:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x2A                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xAC2:0x29 DW_TAG_member
	.asciz	"l_RFile_POSIX_File_POSIX_POSIX"            # DW_AT_name
	.long	0x986                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x2B                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xAEB:0x15 DW_TAG_member
	.asciz	"read_POSIX"                                # DW_AT_name
	.long	0x9FB                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x2C                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xB00:0x19 DW_TAG_member
	.asciz	"installR_POSIX"                            # DW_AT_name
	.long	0xA95                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x2D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0xB1A:0x27 DW_TAG_variable
	.asciz	"stdin_rfile"                               # DW_AT_name
	.asciz	"stdin_rfile"                               # DW_AT_MIPS_linkage_name
	.long	0xA9D                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1E5                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	stdin_rfile                                 # DW_AT_location

	.uleb128	10                                  # Abbrev [10] 0xB41:0x8 DW_TAG_pointer_type
	.long	0xC13                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xB49:0x13 DW_TAG_typedef
	.long	0xB41                                       # DW_AT_type
	.asciz	"WFile_POSIX"                               # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xC                                         # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xB5C:0x1A DW_TAG_subroutine_type
	.long	0xB76                                       # DW_AT_sibling
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xB66:0x5 DW_TAG_formal_parameter
	.long	0xB49                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xB6B:0x5 DW_TAG_formal_parameter
	.long	0x9BF                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xB70:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xB76:0x8 DW_TAG_pointer_type
	.long	0xB5C                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0xB7E:0x8 DW_TAG_pointer_type
	.long	0xBB5                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xB86:0xD DW_TAG_typedef
	.long	0xB7E                                       # DW_AT_type
	.asciz	"CLOS2"                                     # DW_AT_name
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x32                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xB93:0x1A DW_TAG_subroutine_type
	.long	0xBAD                                       # DW_AT_sibling
	.long	0x766                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xB9D:0x5 DW_TAG_formal_parameter
	.long	0xB86                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xBA2:0x5 DW_TAG_formal_parameter
	.long	0x766                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xBA7:0x5 DW_TAG_formal_parameter
	.long	0x766                                       # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xBAD:0x8 DW_TAG_pointer_type
	.long	0xB93                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xBB5:0x2F DW_TAG_structure_type
	.long	0xBE4                                       # DW_AT_sibling
	.asciz	"CLOS2"                                     # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x31                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xBC3:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x6E                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xBD4:0xF DW_TAG_member
	.asciz	"Code"                                      # DW_AT_name
	.long	0xBAD                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x6F                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0xBE4:0xD DW_TAG_typedef
	.long	0xB7E                                       # DW_AT_type
	.asciz	"CLOS2"                                     # DW_AT_name
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0x32                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xBF1:0x1A DW_TAG_subroutine_type
	.long	0xC0B                                       # DW_AT_sibling
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xBFB:0x5 DW_TAG_formal_parameter
	.long	0xB49                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xC00:0x5 DW_TAG_formal_parameter
	.long	0xBE4                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xC05:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xC0B:0x8 DW_TAG_pointer_type
	.long	0xBF1                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xC13:0x7E DW_TAG_structure_type
	.long	0xC91                                       # DW_AT_sibling
	.asciz	"WFile_POSIX"                               # DW_AT_name
	.byte	0x10                                        # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xB                                         # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xC27:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x30                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xC38:0x29 DW_TAG_member
	.asciz	"l_WFile_POSIX_File_POSIX_POSIX"            # DW_AT_name
	.long	0x986                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x31                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xC61:0x16 DW_TAG_member
	.asciz	"write_POSIX"                               # DW_AT_name
	.long	0xB76                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x32                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xC77:0x19 DW_TAG_member
	.asciz	"installW_POSIX"                            # DW_AT_name
	.long	0xC0B                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x33                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0xC91:0x29 DW_TAG_variable
	.asciz	"stdout_wfile"                              # DW_AT_name
	.asciz	"stdout_wfile"                              # DW_AT_MIPS_linkage_name
	.long	0xC13                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1E6                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	stdout_wfile                                # DW_AT_location

	.uleb128	10                                  # Abbrev [10] 0xCBA:0x8 DW_TAG_pointer_type
	.long	0xDA8                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xCC2:0x15 DW_TAG_typedef
	.long	0xCBA                                       # DW_AT_type
	.asciz	"Sockets_POSIX"                             # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1A                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xCD7:0x25 DW_TAG_structure_type
	.long	0xCFC                                       # DW_AT_sibling
	.asciz	"Host_POSIX"                                # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xD                                         # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xCEA:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x36                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xCFC:0x8 DW_TAG_pointer_type
	.long	0xCD7                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xD04:0x12 DW_TAG_typedef
	.long	0xCFC                                       # DW_AT_type
	.asciz	"Host_POSIX"                                # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xE                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xD16:0x25 DW_TAG_structure_type
	.long	0xD3B                                       # DW_AT_sibling
	.asciz	"Port_POSIX"                                # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x11                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xD29:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x3D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xD3B:0x8 DW_TAG_pointer_type
	.long	0xD16                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xD43:0x12 DW_TAG_typedef
	.long	0xD3B                                       # DW_AT_type
	.asciz	"Port_POSIX"                                # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x12                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xD55:0x24 DW_TAG_subroutine_type
	.long	0xD79                                       # DW_AT_sibling
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xD5F:0x5 DW_TAG_formal_parameter
	.long	0xCC2                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD64:0x5 DW_TAG_formal_parameter
	.long	0xD04                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD69:0x5 DW_TAG_formal_parameter
	.long	0xD43                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD6E:0x5 DW_TAG_formal_parameter
	.long	0xBE4                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD73:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xD79:0x8 DW_TAG_pointer_type
	.long	0xD55                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xD81:0x1F DW_TAG_subroutine_type
	.long	0xDA0                                       # DW_AT_sibling
	.long	0x882                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xD8B:0x5 DW_TAG_formal_parameter
	.long	0xCC2                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD90:0x5 DW_TAG_formal_parameter
	.long	0xD43                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD95:0x5 DW_TAG_formal_parameter
	.long	0xBE4                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xD9A:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xDA0:0x8 DW_TAG_pointer_type
	.long	0xD81                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xDA8:0x57 DW_TAG_structure_type
	.long	0xDFF                                       # DW_AT_sibling
	.asciz	"Sockets_POSIX"                             # DW_AT_name
	.byte	0xC                                         # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x19                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xDBE:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x52                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xDCF:0x18 DW_TAG_member
	.asciz	"connect_POSIX"                             # DW_AT_name
	.long	0xD79                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x57                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xDE7:0x17 DW_TAG_member
	.asciz	"listen_POSIX"                              # DW_AT_name
	.long	0xDA0                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x5B                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0xDFF:0x17 DW_TAG_variable
	.asciz	"tcp"                                       # DW_AT_name
	.asciz	"tcp"                                       # DW_AT_MIPS_linkage_name
	.long	0xDA8                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1E8                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	tcp                                         # DW_AT_location

	.uleb128	3                                   # Abbrev [3] 0xE16:0x15 DW_TAG_typedef
	.long	0xCBA                                       # DW_AT_type
	.asciz	"Sockets_POSIX"                             # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1A                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xE2B:0x3D DW_TAG_structure_type
	.long	0xE68                                       # DW_AT_sibling
	.asciz	"Internet_POSIX"                            # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1B                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xE42:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x5E                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xE53:0x14 DW_TAG_member
	.asciz	"tcp_POSIX"                                 # DW_AT_name
	.long	0xE16                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x5F                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0xE68:0x19 DW_TAG_variable
	.asciz	"inet"                                      # DW_AT_name
	.asciz	"inet"                                      # DW_AT_MIPS_linkage_name
	.long	0xE2B                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1EA                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	inet                                        # DW_AT_location

	.uleb128	10                                  # Abbrev [10] 0xE81:0x8 DW_TAG_pointer_type
	.long	0x101A                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xE89:0x11 DW_TAG_typedef
	.long	0xE81                                       # DW_AT_type
	.asciz	"Env_POSIX"                                 # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1E                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xE9A:0x1A DW_TAG_subroutine_type
	.long	0xEB4                                       # DW_AT_sibling
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xEA4:0x5 DW_TAG_formal_parameter
	.long	0xE89                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xEA9:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xEAE:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xEB4:0x8 DW_TAG_pointer_type
	.long	0xE9A                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	7                                   # Abbrev [7] 0xEBC:0xF DW_TAG_array_type
	.long	0xECB                                       # DW_AT_sibling
	.long	0x766                                       # DW_AT_type

	.uleb128	19                                  # Abbrev [19] 0xEC5:0x5 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	13                                  # Abbrev [13] 0xECB:0x3F DW_TAG_structure_type
	.long	0xF0A                                       # DW_AT_sibling
	.asciz	"Array"                                     # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA7                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xED9:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x28E                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA8                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xEEA:0xF DW_TAG_member
	.asciz	"size"                                      # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xA9                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xEF9:0x10 DW_TAG_member
	.asciz	"elems"                                     # DW_AT_name
	.long	0xEBC                                       # DW_AT_type
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xAA                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xF0A:0x8 DW_TAG_pointer_type
	.long	0xECB                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xF12:0xD DW_TAG_typedef
	.long	0xF0A                                       # DW_AT_type
	.asciz	"Array"                                     # DW_AT_name
	.byte	0x12                                        # DW_AT_decl_file
	.byte	0xB0                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xF1F:0x13 DW_TAG_typedef
	.long	0x9CB                                       # DW_AT_type
	.asciz	"RFile_POSIX"                               # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xA                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xF32:0x13 DW_TAG_typedef
	.long	0xB41                                       # DW_AT_type
	.asciz	"WFile_POSIX"                               # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0xC                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xF45:0x28 DW_TAG_structure_type
	.long	0xF6D                                       # DW_AT_sibling
	.asciz	"Maybe_Prelude"                             # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x13                                        # DW_AT_decl_file
	.byte	0x1C                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xF5B:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x13                                        # DW_AT_decl_file
	.byte	0x66                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xF6D:0x8 DW_TAG_pointer_type
	.long	0xF45                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xF75:0x15 DW_TAG_typedef
	.long	0xF6D                                       # DW_AT_type
	.asciz	"Maybe_Prelude"                             # DW_AT_name
	.byte	0x13                                        # DW_AT_decl_file
	.byte	0x1D                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0xF8A:0x1A DW_TAG_subroutine_type
	.long	0xFA4                                       # DW_AT_sibling
	.long	0xF75                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0xF94:0x5 DW_TAG_formal_parameter
	.long	0xE89                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xF99:0x5 DW_TAG_formal_parameter
	.long	0x9BF                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0xF9E:0x5 DW_TAG_formal_parameter
	.long	0x77                                        # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xFA4:0x8 DW_TAG_pointer_type
	.long	0xF8A                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0xFAC:0x3C DW_TAG_structure_type
	.long	0xFE8                                       # DW_AT_sibling
	.asciz	"Time"                                      # DW_AT_name
	.byte	0xC                                         # DW_AT_byte_size
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x8A                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0xFB9:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x28E                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x8B                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xFCA:0xE DW_TAG_member
	.asciz	"sec"                                       # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x8C                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0xFD8:0xF DW_TAG_member
	.asciz	"usec"                                      # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x8D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0xFE8:0x8 DW_TAG_pointer_type
	.long	0xFAC                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0xFF0:0xC DW_TAG_typedef
	.long	0xFE8                                       # DW_AT_type
	.asciz	"Time"                                      # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x90                                        # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0xFFC:0x8 DW_TAG_pointer_type
	.long	0xE2B                                       # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1004:0x16 DW_TAG_typedef
	.long	0xFFC                                       # DW_AT_type
	.asciz	"Internet_POSIX"                            # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1C                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x101A:0xD6 DW_TAG_structure_type
	.long	0x10F0                                      # DW_AT_sibling
	.asciz	"Env_POSIX"                                 # DW_AT_name
	.byte	0x24                                        # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1D                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x102C:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x62                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x103D:0x15 DW_TAG_member
	.asciz	"exit_POSIX"                                # DW_AT_name
	.long	0xEB4                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x63                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1052:0x15 DW_TAG_member
	.asciz	"argv_POSIX"                                # DW_AT_name
	.long	0xF12                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x64                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1067:0x16 DW_TAG_member
	.asciz	"stdin_POSIX"                               # DW_AT_name
	.long	0xF1F                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x65                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x107D:0x17 DW_TAG_member
	.asciz	"stdout_POSIX"                              # DW_AT_name
	.long	0xF32                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x66                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	16                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1094:0x16 DW_TAG_member
	.asciz	"openR_POSIX"                               # DW_AT_name
	.long	0xFA4                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x67                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	20                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x10AA:0x16 DW_TAG_member
	.asciz	"openW_POSIX"                               # DW_AT_name
	.long	0xFA4                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x68                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	24                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x10C0:0x1A DW_TAG_member
	.asciz	"startTime_POSIX"                           # DW_AT_name
	.long	0xFF0                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x69                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	28                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x10DA:0x15 DW_TAG_member
	.asciz	"inet_POSIX"                                # DW_AT_name
	.long	0x1004                                      # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x6A                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	32                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0x10F0:0x25 DW_TAG_variable
	.asciz	"env_struct"                                # DW_AT_name
	.asciz	"env_struct"                                # DW_AT_MIPS_linkage_name
	.long	0x101A                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1EE                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	env_struct                                  # DW_AT_location

	.uleb128	3                                   # Abbrev [3] 0x1115:0x11 DW_TAG_typedef
	.long	0xE81                                       # DW_AT_type
	.asciz	"Env_POSIX"                                 # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x1E                                        # DW_AT_decl_line

	.uleb128	18                                  # Abbrev [18] 0x1126:0x17 DW_TAG_variable
	.asciz	"env"                                       # DW_AT_name
	.asciz	"env"                                       # DW_AT_MIPS_linkage_name
	.long	0x1115                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1F8                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	env                                         # DW_AT_location

	.uleb128	20                                  # Abbrev [20] 0x113D:0x2 DW_TAG_subroutine_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	10                                  # Abbrev [10] 0x113F:0x8 DW_TAG_pointer_type
	.long	0x113D                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	10                                  # Abbrev [10] 0x1147:0x8 DW_TAG_pointer_type
	.long	0x115E                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x114F:0xF DW_TAG_typedef
	.long	0x1147                                      # DW_AT_type
	.asciz	"FunList"                                   # DW_AT_name
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0xD7                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x115E:0x2C DW_TAG_structure_type
	.long	0x118A                                      # DW_AT_sibling
	.asciz	"FunList"                                   # DW_AT_name
	.byte	0x8                                         # DW_AT_byte_size
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0xD6                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x116E:0xC DW_TAG_member
	.asciz	"f"                                         # DW_AT_name
	.long	0x113F                                      # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0xDA                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x117A:0xF DW_TAG_member
	.asciz	"next"                                      # DW_AT_name
	.long	0x114F                                      # DW_AT_type
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0xDB                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	18                                  # Abbrev [18] 0x118A:0x1F DW_TAG_variable
	.asciz	"scanner"                                   # DW_AT_name
	.asciz	"scanner"                                   # DW_AT_MIPS_linkage_name
	.long	0x115E                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x20A                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	scanner                                     # DW_AT_location

	.uleb128	13                                  # Abbrev [13] 0x11A9:0x74 DW_TAG_structure_type
	.long	0x121D                                      # DW_AT_sibling
	.asciz	"__pthread_mutex_s"                         # DW_AT_name
	.byte	0x18                                        # DW_AT_byte_size
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x4F                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x11C3:0x11 DW_TAG_member
	.asciz	"__lock"                                    # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x50                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x11D4:0x12 DW_TAG_member
	.asciz	"__count"                                   # DW_AT_name
	.long	0x4B8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x51                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x11E6:0x12 DW_TAG_member
	.asciz	"__owner"                                   # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x52                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x11F8:0x11 DW_TAG_member
	.asciz	"__kind"                                    # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x58                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1209:0x13 DW_TAG_member
	.asciz	"__nusers"                                  # DW_AT_name
	.long	0x4B8                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x5E                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	16                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	7                                   # Abbrev [7] 0x121D:0x10 DW_TAG_array_type
	.long	0x122D                                      # DW_AT_sibling
	.long	0x596                                       # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x1226:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x17                                        # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	17                                  # Abbrev [17] 0x122D:0x3D DW_TAG_union_type
	.long	0x126A                                      # DW_AT_sibling
	.byte	0x18                                        # DW_AT_byte_size
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x4D                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x1235:0x11 DW_TAG_member
	.asciz	"__data"                                    # DW_AT_name
	.long	0x11A9                                      # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x65                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1246:0x11 DW_TAG_member
	.asciz	"__size"                                    # DW_AT_name
	.long	0x121D                                      # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x66                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1257:0x12 DW_TAG_member
	.asciz	"__align"                                   # DW_AT_name
	.long	0x2C1                                       # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x67                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0x126A:0x17 DW_TAG_typedef
	.long	0x122D                                      # DW_AT_type
	.asciz	"pthread_mutex_t"                           # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x6B                                        # DW_AT_decl_line

	.uleb128	9                                   # Abbrev [9] 0x1281:0x16 DW_TAG_variable
	.asciz	"rts"                                       # DW_AT_name
	.asciz	"rts"                                       # DW_AT_MIPS_linkage_name
	.long	0x126A                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x83                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	rts                                         # DW_AT_location

	.uleb128	7                                   # Abbrev [7] 0x1297:0x10 DW_TAG_array_type
	.long	0x12A7                                      # DW_AT_sibling
	.long	0x596                                       # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x12A0:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x3                                         # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	17                                  # Abbrev [17] 0x12A7:0x2C DW_TAG_union_type
	.long	0x12D3                                      # DW_AT_sibling
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x6B                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x12AF:0x11 DW_TAG_member
	.asciz	"__size"                                    # DW_AT_name
	.long	0x1297                                      # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x6C                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x12C0:0x12 DW_TAG_member
	.asciz	"__align"                                   # DW_AT_name
	.long	0x77                                        # DW_AT_type
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x6D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0x12D3:0x1B DW_TAG_typedef
	.long	0x12A7                                      # DW_AT_type
	.asciz	"pthread_mutexattr_t"                       # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x74                                        # DW_AT_decl_line

	.uleb128	9                                   # Abbrev [9] 0x12EE:0x2C DW_TAG_variable
	.asciz	"glob_mutexattr"                            # DW_AT_name
	.asciz	"glob_mutexattr"                            # DW_AT_MIPS_linkage_name
	.long	0x12D3                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x3                                         # DW_AT_decl_file
	.byte	0x84                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	glob_mutexattr                              # DW_AT_location

	.uleb128	3                                   # Abbrev [3] 0x131A:0x11 DW_TAG_typedef
	.long	0x2FD                                       # DW_AT_type
	.asciz	"__swblk_t"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x9A                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x132B:0x13 DW_TAG_typedef
	.long	0x131A                                      # DW_AT_type
	.asciz	"__blksize_t"                               # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xA4                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x133E:0x12 DW_TAG_typedef
	.long	0x132B                                      # DW_AT_type
	.asciz	"__blkcnt_t"                                # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xA9                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1350:0xD DW_TAG_typedef
	.long	0x133E                                      # DW_AT_type
	.asciz	"off_t"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x58                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x135D:0xF DW_TAG_typedef
	.long	0x1350                                      # DW_AT_type
	.asciz	"clock_t"                                   # DW_AT_name
	.byte	0xD                                         # DW_AT_decl_file
	.byte	0x3C                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x136C:0xE DW_TAG_typedef
	.long	0x135D                                      # DW_AT_type
	.asciz	"time_t"                                    # DW_AT_name
	.byte	0xD                                         # DW_AT_decl_file
	.byte	0x4C                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x137A:0x13 DW_TAG_typedef
	.long	0x136C                                      # DW_AT_type
	.asciz	"suseconds_t"                               # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x8D                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x138D:0x11 DW_TAG_typedef
	.long	0x137A                                      # DW_AT_type
	.asciz	"__fd_mask"                                 # DW_AT_name
	.byte	0x16                                        # DW_AT_decl_file
	.byte	0x37                                        # DW_AT_decl_line

	.uleb128	7                                   # Abbrev [7] 0x139E:0x10 DW_TAG_array_type
	.long	0x13AE                                      # DW_AT_sibling
	.long	0x138D                                      # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x13A7:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x1F                                        # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	16                                  # Abbrev [16] 0x13AE:0x1C DW_TAG_structure_type
	.long	0x13CA                                      # DW_AT_sibling
	.byte	0x80                                        # DW_AT_byte_size
	.byte	0x16                                        # DW_AT_decl_file
	.byte	0x44                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x13B6:0x13 DW_TAG_member
	.asciz	"fds_bits"                                  # DW_AT_name
	.long	0x139E                                      # DW_AT_type
	.byte	0x16                                        # DW_AT_decl_file
	.byte	0x48                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0x13CA:0xE DW_TAG_typedef
	.long	0x13AE                                      # DW_AT_type
	.asciz	"fd_set"                                    # DW_AT_name
	.byte	0x16                                        # DW_AT_decl_file
	.byte	0x4E                                        # DW_AT_decl_line

	.uleb128	9                                   # Abbrev [9] 0x13D8:0x20 DW_TAG_variable
	.asciz	"readUsed"                                  # DW_AT_name
	.asciz	"readUsed"                                  # DW_AT_MIPS_linkage_name
	.long	0x13CA                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x64                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	readUsed                                    # DW_AT_location

	.uleb128	9                                   # Abbrev [9] 0x13F8:0x22 DW_TAG_variable
	.asciz	"writeUsed"                                 # DW_AT_name
	.asciz	"writeUsed"                                 # DW_AT_MIPS_linkage_name
	.long	0x13CA                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x64                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	writeUsed                                   # DW_AT_location

	.uleb128	7                                   # Abbrev [7] 0x141A:0x11 DW_TAG_array_type
	.long	0x142B                                      # DW_AT_sibling
	.long	0xA6E                                       # DW_AT_type

	.uleb128	21                                  # Abbrev [21] 0x1423:0x7 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.short	0x3FF                                       # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x142B:0x1E DW_TAG_variable
	.asciz	"rdTable"                                   # DW_AT_name
	.asciz	"rdTable"                                   # DW_AT_MIPS_linkage_name
	.long	0x141A                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x66                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	rdTable                                     # DW_AT_location

	.uleb128	7                                   # Abbrev [7] 0x1449:0x11 DW_TAG_array_type
	.long	0x145A                                      # DW_AT_sibling
	.long	0xBE4                                       # DW_AT_type

	.uleb128	21                                  # Abbrev [21] 0x1452:0x7 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.short	0x3FF                                       # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x145A:0x1E DW_TAG_variable
	.asciz	"wrTable"                                   # DW_AT_name
	.asciz	"wrTable"                                   # DW_AT_MIPS_linkage_name
	.long	0x1449                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x67                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	wrTable                                     # DW_AT_location

	.uleb128	2                                   # Abbrev [2] 0x1478:0x16 DW_TAG_base_type
	.byte	0x7                                         # DW_AT_encoding
	.asciz	"short unsigned int"                        # DW_AT_name
	.byte	0x2                                         # DW_AT_byte_size

	.uleb128	3                                   # Abbrev [3] 0x148E:0x11 DW_TAG_typedef
	.long	0x1478                                      # DW_AT_type
	.asciz	"__u_short"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x20                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x149F:0x12 DW_TAG_typedef
	.long	0x148E                                      # DW_AT_type
	.asciz	"__uint16_t"                                # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x28                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x14B1:0xF DW_TAG_typedef
	.long	0x149F                                      # DW_AT_type
	.asciz	"u_short"                                   # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x24                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x14C0:0xE DW_TAG_typedef
	.long	0x14B1                                      # DW_AT_type
	.asciz	"ushort"                                    # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x98                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x14CE:0x11 DW_TAG_typedef
	.long	0x14C0                                      # DW_AT_type
	.asciz	"u_int16_t"                                 # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0xCA                                        # DW_AT_decl_line

	.uleb128	5                                   # Abbrev [5] 0x14DF:0x8 DW_TAG_const_type
	.long	0x14CE                                      # DW_AT_type
	.byte	0x2                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x14E7:0x13 DW_TAG_typedef
	.long	0x14DF                                      # DW_AT_type
	.asciz	"sa_family_t"                               # DW_AT_name
	.byte	0x11                                        # DW_AT_decl_file
	.byte	0x1D                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x14FA:0x10 DW_TAG_typedef
	.long	0x14E7                                      # DW_AT_type
	.asciz	"uint16_t"                                  # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x32                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x150A:0x16 DW_TAG_typedef
	.long	0x14FA                                      # DW_AT_type
	.asciz	"uint_least16_t"                            # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x4E                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1520:0x11 DW_TAG_typedef
	.long	0x150A                                      # DW_AT_type
	.asciz	"in_port_t"                                 # DW_AT_name
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0x61                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1531:0xF DW_TAG_typedef
	.long	0x4B8                                       # DW_AT_type
	.asciz	"__u_int"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x21                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1540:0x12 DW_TAG_typedef
	.long	0x1531                                      # DW_AT_type
	.asciz	"__uint32_t"                                # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x2A                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1552:0xF DW_TAG_typedef
	.long	0x1540                                      # DW_AT_type
	.asciz	"__uid_t"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x87                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1561:0xF DW_TAG_typedef
	.long	0x1552                                      # DW_AT_type
	.asciz	"__gid_t"                                   # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x88                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1570:0x10 DW_TAG_typedef
	.long	0x1561                                      # DW_AT_type
	.asciz	"__mode_t"                                  # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x8B                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1580:0x11 DW_TAG_typedef
	.long	0x1570                                      # DW_AT_type
	.asciz	"__nlink_t"                                 # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x8C                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1591:0xE DW_TAG_typedef
	.long	0x1580                                      # DW_AT_type
	.asciz	"__id_t"                                    # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x94                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x159F:0x14 DW_TAG_typedef
	.long	0x1591                                      # DW_AT_type
	.asciz	"__useconds_t"                              # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0x96                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15B3:0x13 DW_TAG_typedef
	.long	0x159F                                      # DW_AT_type
	.asciz	"__socklen_t"                               # DW_AT_name
	.byte	0xE                                         # DW_AT_decl_file
	.byte	0xC0                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15C6:0xD DW_TAG_typedef
	.long	0x15B3                                      # DW_AT_type
	.asciz	"u_int"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x25                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15D3:0xD DW_TAG_typedef
	.long	0x15C6                                      # DW_AT_type
	.asciz	"gid_t"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x43                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15E0:0xE DW_TAG_typedef
	.long	0x15D3                                      # DW_AT_type
	.asciz	"mode_t"                                    # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x48                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15EE:0xF DW_TAG_typedef
	.long	0x15E0                                      # DW_AT_type
	.asciz	"nlink_t"                                   # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x4D                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x15FD:0xD DW_TAG_typedef
	.long	0x15EE                                      # DW_AT_type
	.asciz	"uid_t"                                     # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x52                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x160A:0xC DW_TAG_typedef
	.long	0x15FD                                      # DW_AT_type
	.asciz	"id_t"                                      # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x69                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1616:0x12 DW_TAG_typedef
	.long	0x160A                                      # DW_AT_type
	.asciz	"useconds_t"                                # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x89                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1628:0xE DW_TAG_typedef
	.long	0x1616                                      # DW_AT_type
	.asciz	"size_t"                                    # DW_AT_name
	.byte	0x6                                         # DW_AT_decl_file
	.byte	0xD6                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1636:0xC DW_TAG_typedef
	.long	0x1628                                      # DW_AT_type
	.asciz	"uint"                                      # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0x99                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1642:0x11 DW_TAG_typedef
	.long	0x1636                                      # DW_AT_type
	.asciz	"u_int32_t"                                 # DW_AT_name
	.byte	0xC                                         # DW_AT_decl_file
	.byte	0xCB                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1653:0x15 DW_TAG_typedef
	.long	0x1642                                      # DW_AT_type
	.asciz	"pthread_key_t"                             # DW_AT_name
	.byte	0xA                                         # DW_AT_decl_file
	.byte	0x8C                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1668:0x11 DW_TAG_typedef
	.long	0x1653                                      # DW_AT_type
	.asciz	"socklen_t"                                 # DW_AT_name
	.byte	0x9                                         # DW_AT_decl_file
	.byte	0xF7                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1679:0x10 DW_TAG_typedef
	.long	0x1668                                      # DW_AT_type
	.asciz	"uint32_t"                                  # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x34                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1689:0x16 DW_TAG_typedef
	.long	0x1679                                      # DW_AT_type
	.asciz	"uint_least32_t"                            # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x4F                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x169F:0x15 DW_TAG_typedef
	.long	0x1689                                      # DW_AT_type
	.asciz	"uint_fast16_t"                             # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x6E                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x16B4:0x15 DW_TAG_typedef
	.long	0x169F                                      # DW_AT_type
	.asciz	"uint_fast32_t"                             # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x6F                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x16C9:0x11 DW_TAG_typedef
	.long	0x16B4                                      # DW_AT_type
	.asciz	"uintptr_t"                                 # DW_AT_name
	.byte	0x8                                         # DW_AT_decl_file
	.byte	0x81                                        # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x16DA:0x11 DW_TAG_typedef
	.long	0x16C9                                      # DW_AT_type
	.asciz	"in_addr_t"                                 # DW_AT_name
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0x8D                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x16EB:0x22 DW_TAG_structure_type
	.long	0x170D                                      # DW_AT_sibling
	.asciz	"in_addr"                                   # DW_AT_name
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0x8F                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x16FB:0x11 DW_TAG_member
	.asciz	"s_addr"                                    # DW_AT_name
	.long	0x16DA                                      # DW_AT_type
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0x90                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	2                                   # Abbrev [2] 0x170D:0x11 DW_TAG_base_type
	.byte	0x8                                         # DW_AT_encoding
	.asciz	"unsigned char"                             # DW_AT_name
	.byte	0x1                                         # DW_AT_byte_size

	.uleb128	7                                   # Abbrev [7] 0x171E:0x10 DW_TAG_array_type
	.long	0x172E                                      # DW_AT_sibling
	.long	0x170D                                      # DW_AT_type

	.uleb128	8                                   # Abbrev [8] 0x1727:0x6 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.byte	0x7                                         # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	13                                  # Abbrev [13] 0x172E:0x63 DW_TAG_structure_type
	.long	0x1791                                      # DW_AT_sibling
	.asciz	"sockaddr_in"                               # DW_AT_name
	.byte	0x10                                        # DW_AT_byte_size
	.byte	0xF                                         # DW_AT_decl_file
	.byte	0x5C                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x1742:0x15 DW_TAG_member
	.asciz	"sin_family"                                # DW_AT_name
	.long	0x14E7                                      # DW_AT_type
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0xE3                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1757:0x13 DW_TAG_member
	.asciz	"sin_port"                                  # DW_AT_name
	.long	0x1520                                      # DW_AT_type
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0xE4                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	2                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x176A:0x13 DW_TAG_member
	.asciz	"sin_addr"                                  # DW_AT_name
	.long	0x16EB                                      # DW_AT_type
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0xE5                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x177D:0x13 DW_TAG_member
	.asciz	"sin_zero"                                  # DW_AT_name
	.long	0x171E                                      # DW_AT_type
	.byte	0x10                                        # DW_AT_decl_file
	.byte	0xEB                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x1791:0x8 DW_TAG_pointer_type
	.long	0x17FA                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1799:0x18 DW_TAG_typedef
	.long	0x1791                                      # DW_AT_type
	.asciz	"Connection_POSIX"                          # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x18                                        # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0x17B1:0x1A DW_TAG_subroutine_type
	.long	0x17CB                                      # DW_AT_sibling
	.long	0x29E                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0x17BB:0x5 DW_TAG_formal_parameter
	.long	0x1799                                      # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x17C0:0x5 DW_TAG_formal_parameter
	.long	0xFF0                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x17C5:0x5 DW_TAG_formal_parameter
	.long	0xFF0                                       # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x17CB:0x8 DW_TAG_pointer_type
	.long	0x17B1                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	11                                  # Abbrev [11] 0x17D3:0x1F DW_TAG_subroutine_type
	.long	0x17F2                                      # DW_AT_sibling
	.long	0x29E                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_prototyped

	.uleb128	12                                  # Abbrev [12] 0x17DD:0x5 DW_TAG_formal_parameter
	.long	0x1799                                      # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x17E2:0x5 DW_TAG_formal_parameter
	.long	0x9BF                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x17E7:0x5 DW_TAG_formal_parameter
	.long	0xFF0                                       # DW_AT_type

	.uleb128	12                                  # Abbrev [12] 0x17EC:0x5 DW_TAG_formal_parameter
	.long	0xFF0                                       # DW_AT_type
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x17F2:0x8 DW_TAG_pointer_type
	.long	0x17D3                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x17FA:0x92 DW_TAG_structure_type
	.long	0x188C                                      # DW_AT_sibling
	.asciz	"Connection_POSIX"                          # DW_AT_name
	.byte	0x10                                        # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x17                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x1813:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x4C                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1824:0x32 DW_TAG_member
	.asciz	"l_Connection_POSIX_Closable_POSIX_POSIX"   # DW_AT_name
	.long	0x882                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x4D                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1856:0x1C DW_TAG_member
	.asciz	"established_POSIX"                         # DW_AT_name
	.long	0x17CB                                      # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x4E                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1872:0x19 DW_TAG_member
	.asciz	"neterror_POSIX"                            # DW_AT_name
	.long	0x17F2                                      # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x4F                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	3                                   # Abbrev [3] 0x188C:0x18 DW_TAG_typedef
	.long	0x1791                                      # DW_AT_type
	.asciz	"Connection_POSIX"                          # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x18                                        # DW_AT_decl_line

	.uleb128	13                                  # Abbrev [13] 0x18A4:0x53 DW_TAG_structure_type
	.long	0x18F7                                      # DW_AT_sibling
	.asciz	"SockData"                                  # DW_AT_name
	.byte	0x1C                                        # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x3F                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x18B5:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x28E                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x40                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x18C6:0xF DW_TAG_member
	.asciz	"addr"                                      # DW_AT_name
	.long	0x172E                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x41                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x18D5:0x12 DW_TAG_member
	.asciz	"handler"                                   # DW_AT_name
	.long	0xBE4                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x42                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	20                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x18E7:0xF DW_TAG_member
	.asciz	"conn"                                      # DW_AT_name
	.long	0x188C                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x43                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	24                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x18F7:0x8 DW_TAG_pointer_type
	.long	0x18A4                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x18FF:0x10 DW_TAG_typedef
	.long	0x18F7                                      # DW_AT_type
	.asciz	"SockData"                                  # DW_AT_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x48                                        # DW_AT_decl_line

	.uleb128	7                                   # Abbrev [7] 0x190F:0x11 DW_TAG_array_type
	.long	0x1920                                      # DW_AT_sibling
	.long	0x18FF                                      # DW_AT_type

	.uleb128	21                                  # Abbrev [21] 0x1918:0x7 DW_TAG_subrange_type
	.long	0x24F                                       # DW_AT_type
	.short	0x3FF                                       # DW_AT_upper_bound
	.byte	0x0                                         # End Of Children Mark

	.uleb128	9                                   # Abbrev [9] 0x1920:0x22 DW_TAG_variable
	.asciz	"sockTable"                                 # DW_AT_name
	.asciz	"sockTable"                                 # DW_AT_MIPS_linkage_name
	.long	0x190F                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x68                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	sockTable                                   # DW_AT_location

	.uleb128	9                                   # Abbrev [9] 0x1942:0x2A DW_TAG_variable
	.asciz	"envRootsDirty"                             # DW_AT_name
	.asciz	"envRootsDirty"                             # DW_AT_MIPS_linkage_name
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x6A                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	envRootsDirty                               # DW_AT_location

	.uleb128	9                                   # Abbrev [9] 0x196C:0x1C DW_TAG_variable
	.asciz	"envmut"                                    # DW_AT_name
	.asciz	"envmut"                                    # DW_AT_MIPS_linkage_name
	.long	0x126A                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x70                                        # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	envmut                                      # DW_AT_location

	.uleb128	18                                  # Abbrev [18] 0x1988:0x23 DW_TAG_variable
	.asciz	"startTime"                                 # DW_AT_name
	.asciz	"startTime"                                 # DW_AT_MIPS_linkage_name
	.long	0xFAC                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1EC                                       # DW_AT_decl_line
	.byte	0x5
	.byte	0x3
	.long	startTime                                   # DW_AT_location

	.uleb128	22                                  # Abbrev [22] 0x19AB:0x21 DW_TAG_subprogram
	.asciz	"mkHost"                                    # DW_AT_name
	.asciz	"mkHost"                                    # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x78                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xD04                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin1                               # DW_AT_low_pc
	.long	.Lfunc_end1                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x19CC:0x21 DW_TAG_subprogram
	.asciz	"mkPort"                                    # DW_AT_name
	.asciz	"mkPort"                                    # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x7F                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xD43                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin2                               # DW_AT_low_pc
	.long	.Lfunc_end2                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x19ED:0x27 DW_TAG_subprogram
	.asciz	"close_fun"                                 # DW_AT_name
	.asciz	"close_fun"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xA3                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin3                               # DW_AT_low_pc
	.long	.Lfunc_end3                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x1A14:0x2D DW_TAG_subprogram
	.asciz	"new_Closable"                              # DW_AT_name
	.asciz	"new_Closable"                              # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xAF                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x882                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin4                               # DW_AT_low_pc
	.long	.Lfunc_end4                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x1A41:0x25 DW_TAG_subprogram
	.asciz	"seek_fun"                                  # DW_AT_name
	.asciz	"seek_fun"                                  # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xBA                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin5                               # DW_AT_low_pc
	.long	.Lfunc_end5                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x1A66:0x25 DW_TAG_subprogram
	.asciz	"new_File"                                  # DW_AT_name
	.asciz	"new_File"                                  # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xC8                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x986                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin6                               # DW_AT_low_pc
	.long	.Lfunc_end6                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x1A8B:0x29 DW_TAG_subprogram
	.asciz	"read_descr"                                # DW_AT_name
	.asciz	"read_descr"                                # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xD2                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x9BF                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin7                               # DW_AT_low_pc
	.long	.Lfunc_end7                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x1AB4:0x25 DW_TAG_subprogram
	.asciz	"read_fun"                                  # DW_AT_name
	.asciz	"read_fun"                                  # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xF2                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x9BF                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin8                               # DW_AT_low_pc
	.long	.Lfunc_end8                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	22                                  # Abbrev [22] 0x1AD9:0x2D DW_TAG_subprogram
	.asciz	"installR_fun"                              # DW_AT_name
	.asciz	"installR_fun"                              # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0xF6                                        # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin9                               # DW_AT_low_pc
	.long	.Lfunc_end9                                 # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1B06:0x28 DW_TAG_subprogram
	.asciz	"new_RFile"                                 # DW_AT_name
	.asciz	"new_RFile"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x102                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xF1F                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin10                              # DW_AT_low_pc
	.long	.Lfunc_end10                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1B2E:0x28 DW_TAG_subprogram
	.asciz	"write_fun"                                 # DW_AT_name
	.asciz	"write_fun"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x10D                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin11                              # DW_AT_low_pc
	.long	.Lfunc_end11                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1B56:0x2E DW_TAG_subprogram
	.asciz	"installW_fun"                              # DW_AT_name
	.asciz	"installW_fun"                              # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x11E                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin12                              # DW_AT_low_pc
	.long	.Lfunc_end12                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1B84:0x28 DW_TAG_subprogram
	.asciz	"new_WFile"                                 # DW_AT_name
	.asciz	"new_WFile"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x12B                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xF32                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin13                              # DW_AT_low_pc
	.long	.Lfunc_end13                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1BAC:0x26 DW_TAG_subprogram
	.asciz	"exit_fun"                                  # DW_AT_name
	.asciz	"exit_fun"                                  # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x136                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin14                              # DW_AT_low_pc
	.long	.Lfunc_end14                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1BD2:0x26 DW_TAG_subprogram
	.asciz	"open_fun"                                  # DW_AT_name
	.asciz	"open_fun"                                  # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x13C                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xF75                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin15                              # DW_AT_low_pc
	.long	.Lfunc_end15                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1BF8:0x28 DW_TAG_subprogram
	.asciz	"openR_fun"                                 # DW_AT_name
	.asciz	"openR_fun"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x14D                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xF75                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin16                              # DW_AT_low_pc
	.long	.Lfunc_end16                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1C20:0x28 DW_TAG_subprogram
	.asciz	"openW_fun"                                 # DW_AT_name
	.asciz	"openW_fun"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x15C                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0xF75                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin17                              # DW_AT_low_pc
	.long	.Lfunc_end17                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	13                                  # Abbrev [13] 0x1C48:0xBA DW_TAG_structure_type
	.long	0x1D02                                      # DW_AT_sibling
	.asciz	"Socket_POSIX"                              # DW_AT_name
	.byte	0x18                                        # DW_AT_byte_size
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x15                                        # DW_AT_decl_line

	.uleb128	14                                  # Abbrev [14] 0x1C5D:0x11 DW_TAG_member
	.asciz	"GCINFO"                                    # DW_AT_name
	.long	0x766                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x44                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	0                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1C6E:0x2E DW_TAG_member
	.asciz	"l_Socket_POSIX_Closable_POSIX_POSIX"       # DW_AT_name
	.long	0x882                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x45                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	4                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1C9C:0x1B DW_TAG_member
	.asciz	"remoteHost_POSIX"                          # DW_AT_name
	.long	0xD04                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x46                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	8                                   # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1CB7:0x1B DW_TAG_member
	.asciz	"remotePort_POSIX"                          # DW_AT_name
	.long	0xD43                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x47                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	12                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1CD2:0x17 DW_TAG_member
	.asciz	"inFile_POSIX"                              # DW_AT_name
	.long	0xF1F                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x48                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	16                                  # DW_AT_data_member_location

	.uleb128	14                                  # Abbrev [14] 0x1CE9:0x18 DW_TAG_member
	.asciz	"outFile_POSIX"                             # DW_AT_name
	.long	0xF32                                       # DW_AT_type
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x49                                        # DW_AT_decl_line
	.byte	0x2
	.byte	0x23
	.uleb128	20                                  # DW_AT_data_member_location
	.byte	0x0                                         # End Of Children Mark

	.uleb128	10                                  # Abbrev [10] 0x1D02:0x8 DW_TAG_pointer_type
	.long	0x1C48                                      # DW_AT_type
	.byte	0x4                                         # DW_AT_byte_size
	.byte	0x1                                         # DW_AT_decl_file
	.byte	0x0                                         # DW_AT_decl_line

	.uleb128	3                                   # Abbrev [3] 0x1D0A:0x14 DW_TAG_typedef
	.long	0x1D02                                      # DW_AT_type
	.asciz	"Socket_POSIX"                              # DW_AT_name
	.byte	0x2                                         # DW_AT_decl_file
	.byte	0x16                                        # DW_AT_decl_line

	.uleb128	23                                  # Abbrev [23] 0x1D1E:0x2A DW_TAG_subprogram
	.asciz	"new_Socket"                                # DW_AT_name
	.asciz	"new_Socket"                                # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x16D                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x1D0A                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin18                              # DW_AT_low_pc
	.long	.Lfunc_end18                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1D48:0x2A DW_TAG_subprogram
	.asciz	"new_socket"                                # DW_AT_name
	.asciz	"new_socket"                                # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x17B                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin19                              # DW_AT_low_pc
	.long	.Lfunc_end19                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	24                                  # Abbrev [24] 0x1D72:0x22 DW_TAG_subprogram
	.asciz	"netError"                                  # DW_AT_name
	.asciz	"netError"                                  # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x188                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin20                              # DW_AT_low_pc
	.long	.Lfunc_end20                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	24                                  # Abbrev [24] 0x1D94:0x30 DW_TAG_subprogram
	.asciz	"setupConnection"                           # DW_AT_name
	.asciz	"setupConnection"                           # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x18F                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin21                              # DW_AT_low_pc
	.long	.Lfunc_end21                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1DC4:0x22 DW_TAG_subprogram
	.asciz	"mkAddr"                                    # DW_AT_name
	.asciz	"mkAddr"                                    # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x197                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x77                                        # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin22                              # DW_AT_low_pc
	.long	.Lfunc_end22                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1DE6:0x2C DW_TAG_subprogram
	.asciz	"connect_fun"                               # DW_AT_name
	.asciz	"connect_fun"                               # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1AF                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x596                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin23                              # DW_AT_low_pc
	.long	.Lfunc_end23                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1E12:0x2A DW_TAG_subprogram
	.asciz	"listen_fun"                                # DW_AT_name
	.asciz	"listen_fun"                                # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1CA                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x882                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin24                              # DW_AT_low_pc
	.long	.Lfunc_end24                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	24                                  # Abbrev [24] 0x1E3C:0x2A DW_TAG_subprogram
	.asciz	"kill_handler"                              # DW_AT_name
	.asciz	"kill_handler"                              # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1F2                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin25                              # DW_AT_low_pc
	.long	.Lfunc_end25                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	24                                  # Abbrev [24] 0x1E66:0x2A DW_TAG_subprogram
	.asciz	"scanEnvRoots"                              # DW_AT_name
	.asciz	"scanEnvRoots"                              # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x1FC                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin26                              # DW_AT_low_pc
	.long	.Lfunc_end26                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1E90:0x28 DW_TAG_subprogram
	.asciz	"eventLoop"                                 # DW_AT_name
	.asciz	"eventLoop"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x20F                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x4E2                                       # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin27                              # DW_AT_low_pc
	.long	.Lfunc_end27                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	23                                  # Abbrev [23] 0x1EB8:0x2C DW_TAG_subprogram
	.asciz	"posix_POSIX"                               # DW_AT_name
	.asciz	"posix_POSIX"                               # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x258                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.long	0x1115                                      # DW_AT_type
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin28                              # DW_AT_low_pc
	.long	.Lfunc_end28                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base

	.uleb128	24                                  # Abbrev [24] 0x1EE4:0x24 DW_TAG_subprogram
	.asciz	"startLoop"                                 # DW_AT_name
	.asciz	"startLoop"                                 # DW_AT_MIPS_linkage_name
	.byte	0x1                                         # DW_AT_decl_file
	.short	0x27E                                       # DW_AT_decl_line
	.byte	0x1                                         # DW_AT_prototyped
	.byte	0x1                                         # DW_AT_external
	.long	.Lfunc_begin29                              # DW_AT_low_pc
	.long	.Lfunc_end29                                # DW_AT_high_pc
	.byte	0x1
	.byte	0x54                                        # DW_AT_frame_base
	.byte	0x0                                         # End Of Children Mark
	.byte	0x0                                         # Extra Pad For GDB
	.byte	0x0                                         # Extra Pad For GDB
	.byte	0x0                                         # Extra Pad For GDB
	.byte	0x0                                         # Extra Pad For GDB
.Linfo_end1:

	.section	.debug_abbrev,"",@progbits
.Labbrev_begin:
	.uleb128	1                                   # Abbreviation Code
	.uleb128	17                                  # DW_TAG_compile_unit
	.uleb128	1                                   # CHILDREN_yes
	.uleb128	16                                  # DW_AT_stmt_list
	.uleb128	6                                   # FORM_data4
	.uleb128	37                                  # DW_AT_producer
	.uleb128	8                                   # FORM_string
	.uleb128	19                                  # DW_AT_language
	.uleb128	11                                  # FORM_data1
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	27                                  # DW_AT_comp_dir
	.uleb128	8                                   # FORM_string
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	2                                   # Abbreviation Code
	.uleb128	36                                  # DW_TAG_base_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	62                                  # DW_AT_encoding
	.uleb128	11                                  # FORM_data1
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	3                                   # Abbreviation Code
	.uleb128	22                                  # DW_TAG_typedef
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	4                                   # Abbreviation Code
	.uleb128	53                                  # DW_TAG_volatile_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	5                                   # Abbreviation Code
	.uleb128	38                                  # DW_TAG_const_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	6                                   # Abbreviation Code
	.uleb128	36                                  # DW_TAG_base_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	62                                  # DW_AT_encoding
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	7                                   # Abbreviation Code
	.uleb128	1                                   # DW_TAG_array_type
	.uleb128	1                                   # CHILDREN_yes
	.uleb128	1                                   # DW_AT_sibling
	.uleb128	19                                  # FORM_ref4
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	8                                   # Abbreviation Code
	.uleb128	33                                  # DW_TAG_subrange_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	47                                  # DW_AT_upper_bound
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	9                                   # Abbreviation Code
	.uleb128	52                                  # DW_TAG_variable
	.uleb128	0                                   # CHILDREN_no
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	8199                                # DW_AT_MIPS_linkage_name
	.uleb128	8                                   # FORM_string
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	63                                  # DW_AT_external
	.uleb128	12                                  # FORM_flag
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	2                                   # DW_AT_location
	.uleb128	10                                  # FORM_block1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	10                                  # Abbreviation Code
	.uleb128	15                                  # DW_TAG_pointer_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	11                                  # Abbreviation Code
	.uleb128	21                                  # DW_TAG_subroutine_type
	.uleb128	1                                   # CHILDREN_yes
	.uleb128	1                                   # DW_AT_sibling
	.uleb128	19                                  # FORM_ref4
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	39                                  # DW_AT_prototyped
	.uleb128	12                                  # FORM_flag
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	12                                  # Abbreviation Code
	.uleb128	5                                   # DW_TAG_formal_parameter
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	13                                  # Abbreviation Code
	.uleb128	19                                  # DW_TAG_structure_type
	.uleb128	1                                   # CHILDREN_yes
	.uleb128	1                                   # DW_AT_sibling
	.uleb128	19                                  # FORM_ref4
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	14                                  # Abbreviation Code
	.uleb128	13                                  # DW_TAG_member
	.uleb128	0                                   # CHILDREN_no
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	56                                  # DW_AT_data_member_location
	.uleb128	10                                  # FORM_block1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	15                                  # Abbreviation Code
	.uleb128	15                                  # DW_TAG_pointer_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	16                                  # Abbreviation Code
	.uleb128	19                                  # DW_TAG_structure_type
	.uleb128	1                                   # CHILDREN_yes
	.uleb128	1                                   # DW_AT_sibling
	.uleb128	19                                  # FORM_ref4
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	17                                  # Abbreviation Code
	.uleb128	23                                  # DW_TAG_union_type
	.uleb128	1                                   # CHILDREN_yes
	.uleb128	1                                   # DW_AT_sibling
	.uleb128	19                                  # FORM_ref4
	.uleb128	11                                  # DW_AT_byte_size
	.uleb128	11                                  # FORM_data1
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	18                                  # Abbreviation Code
	.uleb128	52                                  # DW_TAG_variable
	.uleb128	0                                   # CHILDREN_no
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	8199                                # DW_AT_MIPS_linkage_name
	.uleb128	8                                   # FORM_string
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	63                                  # DW_AT_external
	.uleb128	12                                  # FORM_flag
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	5                                   # FORM_data2
	.uleb128	2                                   # DW_AT_location
	.uleb128	10                                  # FORM_block1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	19                                  # Abbreviation Code
	.uleb128	33                                  # DW_TAG_subrange_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	20                                  # Abbreviation Code
	.uleb128	21                                  # DW_TAG_subroutine_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	39                                  # DW_AT_prototyped
	.uleb128	12                                  # FORM_flag
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	21                                  # Abbreviation Code
	.uleb128	33                                  # DW_TAG_subrange_type
	.uleb128	0                                   # CHILDREN_no
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	47                                  # DW_AT_upper_bound
	.uleb128	5                                   # FORM_data2
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	22                                  # Abbreviation Code
	.uleb128	46                                  # DW_TAG_subprogram
	.uleb128	0                                   # CHILDREN_no
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	8199                                # DW_AT_MIPS_linkage_name
	.uleb128	8                                   # FORM_string
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	11                                  # FORM_data1
	.uleb128	39                                  # DW_AT_prototyped
	.uleb128	12                                  # FORM_flag
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	63                                  # DW_AT_external
	.uleb128	12                                  # FORM_flag
	.uleb128	17                                  # DW_AT_low_pc
	.uleb128	1                                   # FORM_addr
	.uleb128	18                                  # DW_AT_high_pc
	.uleb128	1                                   # FORM_addr
	.uleb128	64                                  # DW_AT_frame_base
	.uleb128	10                                  # FORM_block1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	23                                  # Abbreviation Code
	.uleb128	46                                  # DW_TAG_subprogram
	.uleb128	0                                   # CHILDREN_no
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	8199                                # DW_AT_MIPS_linkage_name
	.uleb128	8                                   # FORM_string
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	5                                   # FORM_data2
	.uleb128	39                                  # DW_AT_prototyped
	.uleb128	12                                  # FORM_flag
	.uleb128	73                                  # DW_AT_type
	.uleb128	19                                  # FORM_ref4
	.uleb128	63                                  # DW_AT_external
	.uleb128	12                                  # FORM_flag
	.uleb128	17                                  # DW_AT_low_pc
	.uleb128	1                                   # FORM_addr
	.uleb128	18                                  # DW_AT_high_pc
	.uleb128	1                                   # FORM_addr
	.uleb128	64                                  # DW_AT_frame_base
	.uleb128	10                                  # FORM_block1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	24                                  # Abbreviation Code
	.uleb128	46                                  # DW_TAG_subprogram
	.uleb128	0                                   # CHILDREN_no
	.uleb128	3                                   # DW_AT_name
	.uleb128	8                                   # FORM_string
	.uleb128	8199                                # DW_AT_MIPS_linkage_name
	.uleb128	8                                   # FORM_string
	.uleb128	58                                  # DW_AT_decl_file
	.uleb128	11                                  # FORM_data1
	.uleb128	59                                  # DW_AT_decl_line
	.uleb128	5                                   # FORM_data2
	.uleb128	39                                  # DW_AT_prototyped
	.uleb128	12                                  # FORM_flag
	.uleb128	63                                  # DW_AT_external
	.uleb128	12                                  # FORM_flag
	.uleb128	17                                  # DW_AT_low_pc
	.uleb128	1                                   # FORM_addr
	.uleb128	18                                  # DW_AT_high_pc
	.uleb128	1                                   # FORM_addr
	.uleb128	64                                  # DW_AT_frame_base
	.uleb128	10                                  # FORM_block1
	.uleb128	0                                   # EOM(1)
	.uleb128	0                                   # EOM(2)

	.uleb128	0                                   # EOM(3)
.Labbrev_end:

	.section	.debug_line,"",@progbits
	.long	.Lline_end-.Lline_begin                     # Length of Source Line Info
.Lline_begin:
	.short	0x2                                         # DWARF version number
	.long	.Lline_prolog_end-.Lline_prolog_begin       # Prolog Length
.Lline_prolog_begin:
	.byte	0x1                                         # Minimum Instruction Length
	.byte	0x1                                         # Default is_stmt_start flag
	.byte	0xF6                                        # Line Base Value (Special Opcodes)
	.byte	0xF5                                        # Line Range Value (Special Opcodes)
	.byte	0xA                                         # Special Opcode Base
	.byte	0x0                                         # DW_LNS_copy arg count
	.byte	0x1                                         # DW_LNS_advance_pc arg count
	.byte	0x1                                         # DW_LNS_advance_line arg count
	.byte	0x1                                         # DW_LNS_set_file arg count
	.byte	0x1                                         # DW_LNS_set_column arg count
	.byte	0x0                                         # DW_LNS_negate_stmt arg count
	.byte	0x0                                         # DW_LNS_set_basic_block arg count
	.byte	0x0                                         # DW_LNS_const_add_pc arg count
	.byte	0x1                                         # DW_LNS_fixed_advance_pc arg count
	.asciz	"/home/capitrane/thesis/timber/lib/"        # Directory
	.asciz	"/home/capitrane/installed/timber//share/timberc-1.0.3/lib" # Directory
	.asciz	"/home/capitrane/installed/timber//share/timberc-1.0.3/rtsPOSIX" # Directory
	.asciz	"/usr/include/sys"                          # Directory
	.asciz	"/usr/include"                              # Directory
	.asciz	"/home/capitrane/local/llvm-gcc/lib/gcc/i686-pc-linux-gnu/4.2.1/include" # Directory
	.asciz	"/usr/include/bits"                         # Directory
	.asciz	"/usr/include/netinet"                      # Directory
	.asciz	"/home/capitrane/installed/timber//share/timberc-1.0.3/include" # Directory
	.byte	0x0                                         # End of directories
	.asciz	"POSIX.extern.c"                            # Source
	.uleb128	1                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"POSIX.h"                                   # Source
	.uleb128	2                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"rts.h"                                     # Source
	.uleb128	3                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"ucontext.h"                                # Source
	.uleb128	4                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"signal.h"                                  # Source
	.uleb128	5                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"stddef.h"                                  # Source
	.uleb128	6                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"errno.h"                                   # Source
	.uleb128	5                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"stdint.h"                                  # Source
	.uleb128	5                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"unistd.h"                                  # Source
	.uleb128	5                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"pthreadtypes.h"                            # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"sigset.h"                                  # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"types.h"                                   # Source
	.uleb128	4                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"time.h"                                    # Source
	.uleb128	5                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"types.h"                                   # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"socket.h"                                  # Source
	.uleb128	4                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"in.h"                                      # Source
	.uleb128	8                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"sockaddr.h"                                # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"timber.h"                                  # Source
	.uleb128	9                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"Prelude.h"                                 # Source
	.uleb128	2                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"time.h"                                    # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"netdb.h"                                   # Source
	.uleb128	5                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"select.h"                                  # Source
	.uleb128	4                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"sched.h"                                   # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"sigaction.h"                               # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.asciz	"siginfo.h"                                 # Source
	.uleb128	7                                   # Directory #
	.uleb128	0                                   # Mod date
	.uleb128	0                                   # File size
	.byte	0x0                                         # End of files
.Lline_prolog_end:

	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :120
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel5                                    # Location label
	.byte	0x8B                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :121
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel6                                    # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :122
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel7                                    # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :123
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel8                                    # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :124
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel9                                    # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :121
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel10                                   # Location label
	.byte	0x11                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :127
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel15                                   # Location label
	.byte	0x1A                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :128
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel16                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :129
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel17                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :130
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel18                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :131
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel19                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :128
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel20                                   # Location label
	.byte	0x11                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :163
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel24                                   # Location label
	.byte	0x37                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :164
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel25                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :165
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel26                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :166
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel27                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :167
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel28                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :168
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel29                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :169
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel30                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :170
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel31                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :171
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel32                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :172
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel33                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :175
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel37                                   # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :177
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel38                                   # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :178
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel39                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :179
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel40                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :180
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel41                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :181
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel42                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :177
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel43                                   # Location label
	.byte	0x10                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :186
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel47                                   # Location label
	.byte	0x1D                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :187
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel48                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :189
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel49                                   # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :192
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel50                                   # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :193
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel51                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :195
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel52                                   # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :196
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel53                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :197
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel54                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :190
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel55                                   # Location label
	.byte	0xD                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :200
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel59                                   # Location label
	.byte	0x1E                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :201
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel60                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :202
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel61                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :203
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel62                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :204
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel63                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :205
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel64                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :201
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel65                                   # Location label
	.byte	0x10                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :210
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel70                                   # Location label
	.byte	0x1D                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :212
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel71                                   # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :213
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel72                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :214
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel73                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :215
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel74                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :218
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel75                                   # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :219
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel76                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :220
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel77                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :222
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel78                                   # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :223
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel79                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :237
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel80                                   # Location label
	.byte	0x22                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :226
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel81                                   # Location label
	.byte	0x3                                         # DW_LNS_advance_line
	.sleb128	-11                                 # Line Offset
	.byte	0x1                                         # DW_LNS_copy
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :227
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel82                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :228
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel83                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :229
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel84                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :230
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel85                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :231
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel86                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :225
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel87                                   # Location label
	.byte	0xE                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :233
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel88                                   # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :236
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel89                                   # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :237
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel90                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :226
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel91                                   # Location label
	.byte	0x3                                         # DW_LNS_advance_line
	.sleb128	-11                                 # Line Offset
	.byte	0x1                                         # DW_LNS_copy
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :234
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel92                                   # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :242
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel96                                   # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :243
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel97                                   # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :246
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel101                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :247
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel102                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :248
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel103                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :249
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel104                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :250
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel105                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :251
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel106                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :252
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel107                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :253
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel108                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :254
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel109                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :255
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel110                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :252
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel111                                  # Location label
	.byte	0x11                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :258
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel115                                  # Location label
	.byte	0x1A                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :259
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel116                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :260
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel117                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :261
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel118                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :262
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel119                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :263
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel120                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :264
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel121                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :259
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel122                                  # Location label
	.byte	0xF                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :269
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel127                                  # Location label
	.byte	0x1E                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :271
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel128                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :273
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel129                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :275
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel130                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :276
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel131                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :274
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel132                                  # Location label
	.byte	0x12                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :278
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel133                                  # Location label
	.byte	0x18                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :279
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel134                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :281
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel135                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :272
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel136                                  # Location label
	.byte	0xB                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :283
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel137                                  # Location label
	.byte	0x1F                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :280
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel138                                  # Location label
	.byte	0x11                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :286
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel142                                  # Location label
	.byte	0x1A                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :287
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel143                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :288
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel144                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :289
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel145                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :290
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel146                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :291
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel147                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :292
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel148                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :293
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel149                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :294
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel150                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :295
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel151                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :296
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel152                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :293
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel153                                  # Location label
	.byte	0x11                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :299
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel157                                  # Location label
	.byte	0x1A                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :300
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel158                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :301
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel159                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :302
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel160                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :303
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel161                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :304
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel162                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :305
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel163                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :300
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel164                                  # Location label
	.byte	0xF                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :310
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel167                                  # Location label
	.byte	0x1E                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :311
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel168                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :312
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel169                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :313
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel170                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :316
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel174                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :318
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel175                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :320
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel176                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :321
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel177                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :319
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel178                                  # Location label
	.byte	0x12                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :323
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel179                                  # Location label
	.byte	0x18                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :324
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel180                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :326
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel181                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :327
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel182                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :328
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel183                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :329
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel184                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :325
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel185                                  # Location label
	.byte	0x10                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :326
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel186                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :333
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel190                                  # Location label
	.byte	0x1B                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :334
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel191                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :335
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel192                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :337
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel193                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :338
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel194                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :339
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel195                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :340
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel196                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :341
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel197                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :342
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel198                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :344
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel199                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :345
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel200                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :337
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel201                                  # Location label
	.byte	0xC                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :348
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel205                                  # Location label
	.byte	0x1F                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :349
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel206                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :350
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel207                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :352
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel208                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :353
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel209                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :354
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel210                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :355
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel211                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :356
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel212                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :357
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel213                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :359
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel214                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :360
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel215                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :352
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel216                                  # Location label
	.byte	0xC                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :365
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel220                                  # Location label
	.byte	0x21                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :367
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel221                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :368
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel222                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :369
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel223                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :370
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel224                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :371
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel225                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :372
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel226                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :373
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel227                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :374
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel228                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :375
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel229                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :367
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel230                                  # Location label
	.byte	0xC                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :379
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel234                                  # Location label
	.byte	0x20                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :381
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel235                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :382
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel236                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :383
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel237                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :384
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel238                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :385
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel239                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :386
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel240                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :387
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel241                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :388
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel242                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :389
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel243                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :384
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel244                                  # Location label
	.byte	0xF                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :392
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel249                                  # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :393
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel250                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :394
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel251                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :395
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel252                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :396
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel253                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :397
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel254                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :399
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel259                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :400
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel260                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :401
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel261                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :402
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel262                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :403
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel263                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :404
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel264                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :405
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel265                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :407
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel269                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :408
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel270                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :410
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel271                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :414
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel272                                  # Location label
	.byte	0x18                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :415
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel273                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :413
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel274                                  # Location label
	.byte	0x12                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :417
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel275                                  # Location label
	.byte	0x18                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :419
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel276                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :425
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel277                                  # Location label
	.byte	0x1A                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :426
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel278                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :427
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel279                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :421
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel280                                  # Location label
	.byte	0xE                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :422
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel281                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :431
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel285                                  # Location label
	.byte	0x1D                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :432
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel286                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :435
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel287                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :436
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel288                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :437
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel289                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :438
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel290                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :439
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel291                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :440
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel292                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :441
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel293                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :442
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel294                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :443
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel295                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :445
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel296                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :448
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel297                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :450
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel298                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :451
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel299                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :453
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel300                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :454
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel301                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :450
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel302                                  # Location label
	.byte	0x10                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :458
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel306                                  # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :459
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel307                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :461
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel308                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :462
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel309                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :463
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel310                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :464
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel311                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :465
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel312                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :466
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel313                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :467
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel314                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :468
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel315                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :469
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel316                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :470
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel317                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :471
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel318                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :472
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel319                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :469
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel320                                  # Location label
	.byte	0x11                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :498
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel323                                  # Location label
	.byte	0x31                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :498
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel324                                  # Location label
	.byte	0x1                                         # DW_LNS_copy
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :508
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel329                                  # Location label
	.byte	0x1E                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :509
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel330                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :512
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel331                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :513
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel332                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :514
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel333                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :515
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel334                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :516
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel335                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :517
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel336                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :511
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel337                                  # Location label
	.byte	0xE                                         # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :519
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel338                                  # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :520
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel339                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :527
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel346                                  # Location label
	.byte	0x1B                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :528
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel347                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :529
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel348                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :531
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel349                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :532
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel350                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :535
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel351                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :536
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel352                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :537
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel353                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :539
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel354                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :543
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel355                                  # Location label
	.byte	0x18                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :544
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel356                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :545
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel357                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :546
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel358                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :547
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel359                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :548
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel360                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :549
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel361                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :550
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel362                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :551
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel363                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :552
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel364                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :553
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel365                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :557
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel366                                  # Location label
	.byte	0x18                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :558
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel367                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :559
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel368                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :560
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel369                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :561
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel370                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :562
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel371                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :564
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel372                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :565
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel373                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :567
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel374                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :568
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel375                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :569
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel376                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :570
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel377                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :571
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel378                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :572
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel379                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :573
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel380                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :576
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel381                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :577
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel382                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :579
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel383                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :581
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel384                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :582
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel385                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :583
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel386                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :584
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel387                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :585
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel388                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :588
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel389                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :550
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel390                                  # Location label
	.byte	0x3                                         # DW_LNS_advance_line
	.sleb128	-38                                 # Line Offset
	.byte	0x1                                         # DW_LNS_copy
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :555
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel391                                  # Location label
	.byte	0x19                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :569
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel392                                  # Location label
	.byte	0x22                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :578
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel393                                  # Location label
	.byte	0x1D                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :586
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel394                                  # Location label
	.byte	0x1C                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :600
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel400                                  # Location label
	.byte	0x22                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :601
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel401                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :602
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel402                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :604
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel403                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :605
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel404                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :608
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel405                                  # Location label
	.byte	0x17                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :609
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel406                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :610
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel407                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :611
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel408                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :613
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel409                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :614
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel410                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :616
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel411                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :617
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel412                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :618
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel413                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :620
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel414                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :621
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel415                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :620
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel416                                  # Location label
	.byte	0x13                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :622
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel417                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :624
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel418                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :625
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel419                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :627
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel420                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :628
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel421                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :629
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel422                                  # Location label
	.byte	0x15                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :631
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel423                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :633
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel424                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :616
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel425                                  # Location label
	.byte	0x3                                         # DW_LNS_advance_line
	.sleb128	-17                                 # Line Offset
	.byte	0x1                                         # DW_LNS_copy
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :638
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel429                                  # Location label
	.byte	0x2A                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :640
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel430                                  # Location label
	.byte	0x16                                        # Line Delta
	# /home/capitrane/thesis/timber/lib/ POSIX.extern.c :641
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Llabel431                                  # Location label
	.byte	0x15                                        # Line Delta
	.byte	0x0                                         # Extended Op
	.byte	0x5                                         # Op size
	.byte	0x2                                         # DW_LNE_set_address
	.long	.Lsection_end1                              # Section end label
	.byte	0x0                                         # DW_LNE_end_sequence
	.uleb128	1
	.byte	0x1
.Lline_end:

	.section	.debug_pubnames,"",@progbits
	.long	.Lpubnames_end1-.Lpubnames_begin1           # Length of Public Names Info
.Lpubnames_begin1:
	.short	0x2                                         # DWARF Version
	.long	.Linfo_begin1                               # Offset of Compilation Unit Info
	.long	.Linfo_end1-.Linfo_begin1                   # Compilation Unit Length
	.long	0x1B84                                      # DIE offset
	.asciz	"new_WFile"                                 # External Name
	.long	0x13D8                                      # DIE offset
	.asciz	"readUsed"                                  # External Name
	.long	0x1DC4                                      # DIE offset
	.asciz	"mkAddr"                                    # External Name
	.long	0x1AD9                                      # DIE offset
	.asciz	"installR_fun"                              # External Name
	.long	0x1942                                      # DIE offset
	.asciz	"envRootsDirty"                             # External Name
	.long	0x1988                                      # DIE offset
	.asciz	"startTime"                                 # External Name
	.long	0x83E                                       # DIE offset
	.asciz	"stdin_cl"                                  # External Name
	.long	0x1EE4                                      # DIE offset
	.asciz	"startLoop"                                 # External Name
	.long	0x1A14                                      # DIE offset
	.asciz	"new_Closable"                              # External Name
	.long	0x93A                                       # DIE offset
	.asciz	"stdin_file"                                # External Name
	.long	0x19ED                                      # DIE offset
	.asciz	"close_fun"                                 # External Name
	.long	0x3BF                                       # DIE offset
	.asciz	"evMsg"                                     # External Name
	.long	0x73A                                       # DIE offset
	.asciz	"__GC__CloseMsg"                            # External Name
	.long	0x19CC                                      # DIE offset
	.asciz	"mkPort"                                    # External Name
	.long	0x19AB                                      # DIE offset
	.asciz	"mkHost"                                    # External Name
	.long	0x6F6                                       # DIE offset
	.asciz	"__GC__DescClosable"                        # External Name
	.long	0x1E3C                                      # DIE offset
	.asciz	"kill_handler"                              # External Name
	.long	0x1E90                                      # DIE offset
	.asciz	"eventLoop"                                 # External Name
	.long	0x12EE                                      # DIE offset
	.asciz	"glob_mutexattr"                            # External Name
	.long	0x13F8                                      # DIE offset
	.asciz	"writeUsed"                                 # External Name
	.long	0x1B06                                      # DIE offset
	.asciz	"new_RFile"                                 # External Name
	.long	0x1126                                      # DIE offset
	.asciz	"env"                                       # External Name
	.long	0x1BD2                                      # DIE offset
	.asciz	"open_fun"                                  # External Name
	.long	0x145A                                      # DIE offset
	.asciz	"wrTable"                                   # External Name
	.long	0x1D48                                      # DIE offset
	.asciz	"new_socket"                                # External Name
	.long	0x262                                       # DIE offset
	.asciz	"__GC__SockData"                            # External Name
	.long	0x1D94                                      # DIE offset
	.asciz	"setupConnection"                           # External Name
	.long	0x1E66                                      # DIE offset
	.asciz	"scanEnvRoots"                              # External Name
	.long	0xC91                                       # DIE offset
	.asciz	"stdout_wfile"                              # External Name
	.long	0x1DE6                                      # DIE offset
	.asciz	"connect_fun"                               # External Name
	.long	0x1BAC                                      # DIE offset
	.asciz	"exit_fun"                                  # External Name
	.long	0x1D72                                      # DIE offset
	.asciz	"netError"                                  # External Name
	.long	0x1AB4                                      # DIE offset
	.asciz	"read_fun"                                  # External Name
	.long	0x1EB8                                      # DIE offset
	.asciz	"posix_POSIX"                               # External Name
	.long	0xB1A                                       # DIE offset
	.asciz	"stdin_rfile"                               # External Name
	.long	0x95F                                       # DIE offset
	.asciz	"stdout_file"                               # External Name
	.long	0x118A                                      # DIE offset
	.asciz	"scanner"                                   # External Name
	.long	0x1A41                                      # DIE offset
	.asciz	"seek_fun"                                  # External Name
	.long	0x1C20                                      # DIE offset
	.asciz	"openW_fun"                                 # External Name
	.long	0x142B                                      # DIE offset
	.asciz	"rdTable"                                   # External Name
	.long	0x1A8B                                      # DIE offset
	.asciz	"read_descr"                                # External Name
	.long	0xE68                                       # DIE offset
	.asciz	"inet"                                      # External Name
	.long	0x1E12                                      # DIE offset
	.asciz	"listen_fun"                                # External Name
	.long	0x1B56                                      # DIE offset
	.asciz	"installW_fun"                              # External Name
	.long	0x1920                                      # DIE offset
	.asciz	"sockTable"                                 # External Name
	.long	0x1A66                                      # DIE offset
	.asciz	"new_File"                                  # External Name
	.long	0x85F                                       # DIE offset
	.asciz	"stdout_cl"                                 # External Name
	.long	0x10F0                                      # DIE offset
	.asciz	"env_struct"                                # External Name
	.long	0x1B2E                                      # DIE offset
	.asciz	"write_fun"                                 # External Name
	.long	0x196C                                      # DIE offset
	.asciz	"envmut"                                    # External Name
	.long	0x6C8                                       # DIE offset
	.asciz	"maxDesc"                                   # External Name
	.long	0x1D1E                                      # DIE offset
	.asciz	"new_Socket"                                # External Name
	.long	0x1281                                      # DIE offset
	.asciz	"rts"                                       # External Name
	.long	0xDFF                                       # DIE offset
	.asciz	"tcp"                                       # External Name
	.long	0x6A2                                       # DIE offset
	.asciz	"eventThread"                               # External Name
	.long	0x1BF8                                      # DIE offset
	.asciz	"openR_fun"                                 # External Name
	.long	0x0                                         # End Mark
.Lpubnames_end1:

	.section	.debug_loc,"",@progbits

	.section	.debug_aranges,"",@progbits

	.section	.debug_ranges,"",@progbits

	.section	.debug_macinfo,"",@progbits


	.section	.note.GNU-stack,"",@progbits
