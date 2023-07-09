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

