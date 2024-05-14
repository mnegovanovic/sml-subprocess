structure Popen :> sig
    (* Parent wants to write, read stdout, or read stdout + stderr *)
    datatype pipe_type = PIPE_W | PIPE_R | PIPE_RE
    val popen: string * pipe_type -> Posix.IO.file_desc option
    val pclose: Posix.IO.file_desc -> Posix.Process.exit_status option
    val getPid: Posix.IO.file_desc -> int option
    val exec: string -> int option * string option
end =

struct
    open MNUtils
    
    datatype pipe_type = PIPE_W | PIPE_R | PIPE_RE
    type pinfo = { fd : Posix.ProcEnv.file_desc, pid : Posix.Process.pid }
    val pids : pinfo list ref = ref []

    (* Implements popen(3) *)
    fun popen (cmd, t) =
        let
            val { infd = readfd, outfd = writefd } = Posix.IO.pipe ()
        in
            case (Posix.Process.fork (), t) of
                (NONE, t) => (* Child *)
                    (( case t of
                            PIPE_W =>
                                Posix.IO.dup2 { old = readfd, new = Posix.FileSys.stdin }
		                    | PIPE_R =>
                                Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stdout }
		                    | PIPE_RE => (
                                Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stdout };
                                Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stderr });
                       Posix.IO.close writefd;
	                   Posix.IO.close readfd;
	                   Posix.Process.execp ("/bin/sh", ["sh", "-c", cmd]))
                       handle OS.SysErr (_, _) => NONE)
                | (SOME pid, t) => (* Parent *)
                    let
                        val fd = case t of
                            PIPE_W => (Posix.IO.close readfd; writefd)
                            | PIPE_R => (Posix.IO.close writefd; readfd)
				            | PIPE_RE => (Posix.IO.close writefd; readfd)
		                val _ = pids := ({ fd = fd, pid = pid } :: !pids)
	                in
                        SOME fd
                    end
        end

    (* Implements pclose(3) *)
    fun pclose fd =
        case List.partition (fn { fd = f, pid = _ } => f = fd) (!pids) of
            ([], _) => NONE
        | ([{ fd = _, pid = pid }], pids') =>
	        let val _ = pids := pids'
	            val (_, status) = Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
	            val _ = Posix.IO.close fd
	        in
                SOME status
            end
        | _ => raise Bind (* This should be impossible. *)

    (* execute command in a new process and return exit code and output *)
    fun exec cmd =
        let
            val fd_opt  = popen (cmd, PIPE_R)
            
        in
            case fd_opt of
                NONE  => (NONE, NONE)
                | SOME fd =>
                    let
                        fun read 0 acc =
                                let
                                    val buf = Posix.IO.readVec (fd, 1024)
                                    val bytes_read = Word8Vector.length buf
                                in
                                    if bytes_read = 0 then
                                        SOME ""
                                    else
                                        read bytes_read (buf::acc)
                                end
                            | read 1024 acc =
                                let
                                    val buf = Posix.IO.readVec (fd, 1024)
                                    val bytes_read = Word8Vector.length buf
                                in
                                    read bytes_read (buf::acc)
                                end
                            | read _ acc = SOME (Byte.bytesToString (Word8Vector.concat (reverse acc)))
                        
                        fun exitCode status_opt =
                            case status_opt of
                                SOME status =>
                                    (case status of
                                        Posix.Process.W_EXITED => SOME 0
                                        | Posix.Process.W_EXITSTATUS sts => SOME (Word8.toInt sts)
                                        | _ => NONE)
                                | NONE => NONE

                        val out = read 0 []
                        val status_opt = pclose fd

                    in
                        (exitCode status_opt, out)
                    end
        end

    (* get pid of the forked process *)
    fun getPid (fd: Posix.ProcEnv.file_desc): int option =
        let
            val pinfo = List.filter (fn { fd = f, pid = _ } => f = fd) (!pids)
        in
            if (List.length pinfo) = 1 then
                SOME (LargeWord.toInt (Posix.Process.pidToWord (#pid (List.hd pinfo))))
            else
                NONE
        end
end

