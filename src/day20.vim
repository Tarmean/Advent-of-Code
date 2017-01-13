sort n
norm! O0
s/\v(\d+)\n(\d+)-(\d+)/\=(str2nr(submatch(2))>str2nr(submatch(1))+1 ? (str2nr(submatch(1))+1."\r"):"") . max([str2nr(submatch(1)), str2nr(submatch(3))])
let @q="&@q"
norm! @q
2,$s/\_.*/\=line("$")-1
