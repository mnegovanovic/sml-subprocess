open MNUtils

val _ = test "test-empty-out" 0 Int.toString (fn () =>
    let
        val (status, _) = Popen.exec "exit 0;"
    in
        valOf status
    end)

val _ = test "test-short-out" "Hello World!\n" (fn x => x) (fn () =>
    let
        val (_, out) = Popen.exec "echo 'Hello World!'; exit 0;"
    in
        valOf out
    end)

fun generateLongString acc =
    if (String.size acc) > (1024 * 10) then
        acc
    else
        generateLongString (acc ^ "Lorem ipsum ")
val long_string = generateLongString ""
val _ = test "test-long-out" (long_string^"\n") (fn x => x) (fn () =>
    let
        val (_, out) = Popen.exec ("echo '" ^ long_string ^ "'; exit 0;")
    in
        valOf out
    end)

val _ = test "test-zero-pid" 0 Int.toString (fn () =>
    let
        val (pid, _) = Popen.exec "echo 'Hello World!'; exit 0;"
    in
        valOf pid
    end)

val _ = test "test-non-zero-pid" 42 Int.toString (fn () =>
    let
        val (pid, _) = Popen.exec "echo 'Hello World!'; exit 42;"
    in
        valOf pid
    end)

