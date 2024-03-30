define i1 @const_args(ptr %input, ptr %return_ptr, %clt* %call_list, i8** %block) {
  %1 = load i8*, i8** %block
  indirectbr i8* %1, [label %.block0, label %.block1, label %.blockblocked ]

.block0:
  %2 = getelementptr {i32, ptr}, ptr null, i32 1
  %3 = ptrtoint ptr %2 to i32
  call void @llvm.memcpy.p0.p0.i32(ptr %return_ptr, ptr %input, i32 %3, i1 0)
  store i8* blockaddress(@const_args, %.block1), i8** %block
  ret i1 1

.block1:
  br label %.blockblocked ; break equivalent

.blockblocked:
  ; make sure we are blocked - we can now just jump to .blockblocked to immediately block ourselves
  store i8* blockaddress(@const_args, %.blockblocked), i8** %block
  ret i1 0
}

@pipeline_main = internal constant [2 x %clt*] [%clt* @stream_main, %clt* @const_args]

%pipeline_stack_main = type { %stream_main_locals, i32 }

define i32 @main(i32 %argc, i8** %argv) noinline {
  %1 = alloca i32

  ; init pipeline
  %2 = getelementptr %clt, %clt* @pipeline_main, i32 1

  %3 = load ptr, ptr @pipeline_main

  %4 = alloca %pipeline_stack_main
  %5 = alloca [2 x i8*]

  %6 = getelementptr i8*, i8** %5, i32 0
  store i8* blockaddress(@stream_main, %.block0), i8** %6
  %7 = getelementptr i8*, i8** %5, i32 1
  store i8* blockaddress(@const_args, %.block0), i8** %7

  ; init args input
  %8 = alloca {i32,ptr}
  %9 = getelementptr {i32,ptr}, ptr %8, i32 0, i32 0
  %10 = getelementptr {i32,ptr}, ptr %8, i32 0, i32 1
  store i32 %argc, ptr %9
  store ptr %argv, ptr %10

  ; call main stream
  %11 = call i1 %3(ptr %4, ptr %1, %clt* %2, i8** %5)

  ; load output
  br i1 %11, label %success, label %fail

success:
  %12 = load i32, ptr %1
  ret i32 %12

fail:
  ret i32 1
}

declare void @llvm.memcpy.p0.p0.i32(ptr, ptr, i32, i1)
