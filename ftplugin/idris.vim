" {{{ boilerplate
if bufname('%') == "idris-response"
  finish
endif

if exists("b:did_ftplugin")
  finish
endif

let idris_response = 0
let b:did_ftplugin = 1

" }}}

" {{{ Recommended settings

setlocal shiftwidth=2
setlocal tabstop=2
if !exists("g:idris_allow_tabchar") || g:idris_allow_tabchar == 0
	setlocal expandtab
endif
setlocal comments=s1:{-,mb:-,ex:-},:\|\|\|,:--
setlocal commentstring=--%s
setlocal iskeyword+=?
setlocal wildignore+=*.ibc

" The connection mamagement logic.
if exists("g:idris_default_prompt")
    let s:idris_prompt = g:idris_default_prompt
else
    let s:idris_prompt = "idris --ide-mode"
endif

if !exists("g:IdrDebugMsg")
    let g:IdrDebugMsg = 1
endif

function! IdrisDocFold(lineNum)
  let line = getline(a:lineNum)

  if line =~ "^\s*|||"
    return "1"
  endif

  return "0"
endfunction

function! IdrisFold(lineNum)
  return IdrisDocFold(a:lineNum)
endfunction

setlocal foldmethod=expr
setlocal foldexpr=IdrisFold(v:lnum)
" }}}

" {{{ Connection protocol
let s:job = v:null
let s:output = ""
let s:protocol_version = 0
let s:after_connection = []
let s:pending_requests = {}
let s:loaded_file = ""
let s:next_message_id = 1

function! IdrisStatus()
    if s:job == v:null
        return "closed"
    else
        return "open"
    endif
endfunction

function! IdrisPending()
    return s:pending_requests
endfunction

function! s:IdrisHandle(channel, msg, event)
  let s:output .= join(a:msg, '')
  if g:IdrDebugMsg
      echom "REC: ".s:output
  endif
  while 6 <= strlen(s:output)
    let kount = str2nr(strpart(s:output, 0, 6), 16)
    if kount + 6 <= strlen(s:output)
        let data = strpart(s:output, 6, kount)
        let s:output = strpart(s:output, 6+kount)
        call s:IdrisMessage(s:FromSExpr(data))
    else
        break
    endif
  endwhile
endfunction

let s:callbacks = {
\ 'on_stdout' : function('s:IdrisHandle'),
\ 'on_stderr' : function('s:IdrisHandle'),
\ }

function! IdrisConnect()
    let base_cmd = split(s:idris_prompt)[0]
    silent! let idris_path = system('command -v '.base_cmd)
    if idris_path !~ '\w\+'
        call IWrite(base_cmd." not found")
        return 0
    endif
    let s:job = jobstart(s:idris_prompt, s:callbacks)
    if s:InIdris2
        " a bit of an abuse
        call IdrisSendMessage([{'command': "enable-syntax :False"}])
    endif
endfunction

function! IdrisReconnect(prompt)
    if IdrisStatus() == "open"
        call IdrisDisconnect()
    endif
    let s:idris_prompt = a:prompt
    call IdrisConnect()
endfunction

function! IdrisDisconnect()
    call jobstop(s:job)
    let s:output = ""
    let s:protocol_version = 0
    let s:pending_requests = {}
    let s:loaded_file = ""
    let s:next_message_id = 1
    let s:after_connection = []
endfunction
" }}}
function! s:filterErrors(text)
    " add line breaks to make errors more readable
    let text = substitute(a:text, '\.idr:[-0-9:]\{4,}\.\zs \zeWhen', '', '')
    return text
endfunction


" {{{ Message syntax
" Message handling
function! s:IdrisMessage(msg)
    if s:IsSyntaxError(a:msg)
        call IAppend("*** protocol syntax error ***")
        call IAppend(printf("message: %s", a:msg.msg))
        call IAppend(printf("context: %s", a:msg.context))
        call IAppend(printf("sexpr: %s", a:msg.sexpr))
        call IAppend(printf("position: %d (before): %s", a:msg.syntax_error, strpart(a:msg.msg, a:msg.syntax_error)))
        return 0
    endif
    if !s:IsList(a:msg)
        echoerr printf("idris ide message that is not a list: %s", a:msg)
        return 0
    endif
    if !s:IsCommand(a:msg[0])
        echoerr printf("idris ide message that is not a command: %s", a:msg)
        return 0
    endif
    let name = a:msg[0]['command']
    if name == 'return' && s:IsNumber(a:msg[2])
        if has_key(s:pending_requests, a:msg[2])
            let req = remove(s:pending_requests, a:msg[2])
            call req.ok(req, a:msg[1])
        else
            " echoerr printf("request '%s' had a response: %s", a:msg[2], a:msg[1])
        endif
    elseif name == 'output' && s:IsNumber(a:msg[2])
        " call IAppend(printf("%s", a:msg[1]))
    elseif name == 'protocol-version' && s:IsNumber(a:msg[1])
        let s:protocol_version = a:msg[1]
        for Item in s:after_connection
            call Item()
        endfor
        let s:after_connection = []
    elseif name == 'set-prompt' && s:IsString(a:msg[1])
        call IAppend(printf("[%s]", a:msg[1]))
    elseif name == 'warning'
        call IAppend(s:filterErrors(a:msg[1][3]))
    elseif name == 'write-string'
        call IAppend(printf("%s", a:msg[1]))
    else
        echoerr printf("unknown idris ide message: %s", a:msg)
    endif
endfunction

function! s:SendingGuard(resumption)
    if IdrisStatus() != "open"
        call IWrite(printf("Starting up: %s", s:idris_prompt))
        call add(s:after_connection, a:resumption)
        call IdrisConnect()
        return 0
    elseif s:protocol_version == 0
        call add(s:after_connection, a:resumption)
        return 0
    else
        return 1
    endif
endfunction

" Protocol encoding/decoding routines
function! s:Write6HexMessage(msg)
    let kount = printf('%06x', strlen(a:msg))
    if g:IdrDebugMsg
        echom "SEND:".kount.a:msg
    endif
    call chansend(s:job, kount . a:msg)
endfunction

"   {{{ S:Expr
function! s:FromSExpr(msg)
    let start = 0
    let sexpr = []
    let context = []
    while 1
        let start = start + strlen(matchstr(a:msg, '^\(\_s\|\)\+', start))
        let head = matchstr(a:msg, '^:[:\-a-zA-Z]\+\|^(\|^)\|^\d\+\|^nil\|^"', start)
        if head =~ '^('
            call add(context, sexpr)
            let sexpr = []
        elseif head =~ '^)' && len(context) > 0
            let top = remove(context, -1)
            call add(top, sexpr)
            let sexpr = top
        elseif head =~ '^:'
            call add(sexpr, {'command':strpart(head, 1)})
        elseif head =~ '^\d'
            call add(sexpr, str2nr(head))
        elseif head =~ '^"'
            let top = match(a:msg, '\\\@<!"', start+1)
            if top <= start
                return {'syntax_error':start, 'sexpr':sexpr, 'context':context, 'msg':(a:msg)}
            else
                let head = strpart(a:msg, start, 1+top-start)
                let item = substitute(strpart(head, 1, strlen(head) - 2), '\\"', '"', 'g')
            endif
            call add(sexpr, item)
        elseif head =~ '^nil'
            call add(sexpr, [])
        elseif start == strlen(a:msg) && len(context) == 0
            if len(sexpr) == 1
                return remove(sexpr, 0)
            else
                return {'syntax_error':start, 'sexpr':sexpr, 'context':context, 'msg':(a:msg)}
            endif
        else
            return {'syntax_error':start, 'sexpr':sexpr, 'context':context, 'msg':(a:msg)}
        endif
        let start = start + strlen(head)
    endwhile
endfunction

function! s:IsSyntaxError(item)
    if type(a:item) == v:t_dict
        return has_key(a:item, 'syntax_error')
    endif
    return 0
endfunction

function! s:IsList(item)
    return type(a:item) == v:t_list
endfunction

function! s:IsString(item)
    return type(a:item) == v:t_string
endfunction

function! s:IsNumber(item)
    return type(a:item) == v:t_number
endfunction

function! s:IsCommand(item)
    if type(a:item) == v:t_dict
        return has_key(a:item, 'command')
    endif
endfunction

function! s:ToSExpr(item)
    if s:IsList(a:item)
        let forms = []
        for subitem in a:item
            call add(forms, s:ToSExpr(subitem))
        endfor
        return '(' . join(forms, ' ') . ')'
    elseif s:IsString(a:item)
        return '"' . substitute(a:item, '"', '\"', 'g') . '"'
    elseif s:IsNumber(a:item)
        return printf("%d", a:item)
    elseif s:IsCommand(a:item)
        return ":" . (a:item)['command']
    endif
endfunction

" }}}
" }}}

" {{{ IDE Commands
" {{{ Communication
function! IdrisSendMessage(command)
    let this_message_id = s:next_message_id
    let msg = s:ToSExpr([a:command, this_message_id]) . "\n"
    let s:next_message_id = s:next_message_id + 1
    call s:Write6HexMessage(msg)
    return this_message_id
endfunction

function! IdrisRequest(command, req)
    let s:pending_requests[s:next_message_id] = a:req
    call IdrisSendMessage(a:command)
endfunction

" }}}

" {{{ Permission logic
" may be removed: only interested in Idris2

" The first and second Idris protocol were simple enough that
" this can be rewritten on subsequent releases.
let s:InAnyIdris = 0
let s:InIdris1 = 1
let s:InIdris2 = 2
function! s:IdrisCmd(...)
    if s:SendingGuard(function("s:IdrisCmd", a:000))
        let argc = a:0
        let suppose = a:1
        let cmdname = a:2
        let req = a:000[argc-1]
        let current_version = s:protocol_version
        if suppose != 0 && current_version != suppose
            echoerr printf("'%s' does not support :%s", s:idris_prompt, cmdname)
        else
            let form = [{'command':cmdname}]
            for item in a:000[2:argc-2]
                call add(form, item)
            endfor
            if type(req) == v:null
                call IdrisSendMessage(form)
            else
                call IdrisRequest(form, req)
            endif
        endif
    endif
endfunction

" }}}

" {{{ IdrisResponse
function! IdrisResponseWin()
  if (!bufexists("idris-response"))
    botright 10split
    badd idris-response
    b idris-response
    let g:idris_respwin = "active"
    set buftype=nofile
    set bufhidden=hide
    wincmd k
  elseif (bufexists("idris-response") && g:idris_respwin == "hidden")
    botright 10split
    b idris-response
    let g:idris_respwin = "active"
    wincmd k
  endif
endfunction

function! IdrisHideResponseWin()
  let g:idris_respwin = "hidden"
endfunction

function! IdrisShowResponseWin()
  let g:idris_respwin = "active"
endfunction

function! IWrite(str)
  if (bufexists("idris-response"))
    let save_cursor = getcurpos()
    b idris-response
    %delete
    let resp = split(a:str, '')
    let n = len(resp)
    let c = 0
    while c < n
        call setbufline("idris-response", c+1, resp[c])
        let c = c + 1
    endwhile
    b #
    call setpos('.', save_cursor)
  else
    echo a:str
  endif
endfunction

function! IAppend(str)
  if (bufexists("idris-response"))
    let win_view = winsaveview()
    let save_cursor = getcurpos()
    b idris-response
    let resp = split(a:str, '')
    call append(line('$'), resp)
    b #
    call setpos('.', save_cursor)
    call winrestview(win_view)
  endif
endfunction
" }}}

" {{{ Commands
" {{{ Command helpers
"
" Text near cursor position that needs to be passed to a command.
function! s:currentQueryObject()
  let word = expand('<cWORD>')
  " First, check whether we are over an operator
  if word =~ '^[^a-zA-Z0-9]\+$'
      "I dris won't recognize an operator wrapped in parentheses
      if word =~ '^(.\+)$'
          let word = word[1:-2]
      endif
  else
    let word = expand("<cword>")
    if word =~ '^?'
      " Cut off '?' that introduces a hole identifier.
      let word = strpart(word, 1)
    endif
  endif
  return word
endfunction

function! s:generic_response(req, command)
    let name = a:command[0]['command']
    if name == 'ok'
        let cmd = a:command[1]
        let response = s:IsList(cmd) ? cmd : split(cmd, '')
        call a:req.on_success(a:req, response)
    elseif name == "error"
        let text = s:filterErrors(a:command[1])
        call IWrite(text)
    else
        echom "unknown idris-response: ".name
    endif
endfunction

function! s:mkGeneric(onSuccess_fname, ...)
    let req = { 'ok': function('s:generic_response'),
                \ 'on_success': function(a:onSuccess_fname) }
    let extra_args = get(a:000, 0, {})
    return extend(req, extra_args)
endfunction

function! s:PrintToBufferResponse(req, response)
    " re-add explicit newlines
    let text = join(a:response, '')
    if strlen(text) == 0
        let text = "ok"
    endif
    call IWrite(text)
endfunction

let s:print_response = s:mkGeneric('s:PrintToBufferResponse')
" }}}

" {{{ Reloading
function! IdrisReload(q)
  w
  let file = expand("%:p")
  call s:IdrisCmd(s:InAnyIdris, "load-file", file, s:print_response)
endfunction

function! s:IdrisMustReload()
    let s:loaded_file = ""
endfunction

function! IdrisReloadGuard(reaction)
  let file = expand("%:p")
  if &modified || s:loaded_file != file
      if &modified
          w
      endif
      call s:IdrisCmd(s:InAnyIdris, "load-file", file, {'ok':function("s:IdrisReloadGuardResponse", [a:reaction, file])})
      return 0
  else
      return 1
  endif
endfunction

function! s:IdrisReloadGuardResponse(reaction, file, req, command)
    let name = a:command[0]['command']
    if name == 'ok'
        let s:loaded_file = a:file
        call a:reaction()
    else
        " echoerr a:command[1]
    endif
endfunction
" }}}
" {{{ *print info
function! s:CurrWordInfo(command)
    let word = s:currentQueryObject()
    call s:IdrisCmd(s:InAnyIdris, a:command, word, s:print_response)
endfunction

function! IdrisShowType()
  if IdrisReloadGuard(function("IdrisShowType"))
    call s:CurrWordInfo("type-of")
  endif
endfunction

function! IdrisShowDoc()
  if IdrisReloadGuard(function("IdrisShowDoc"))
    call s:CurrWordInfo("docs-for")
  endif
endfunction

function! IdrisPrintDef()
  " upstream not-implemented
  if IdrisReloadGuard(function("IdrisPrintDef"))
    call s:CurrWordInfo("print-definition")
  endif
endfunction

let s:last_namespace = ""
function! IdrisBrowseNamespace()
  if IdrisReloadGuard(function("IdrisBrowseNamespace"))
    let word = expand("<cWORD>")
    " At least one division, and each submodule starts with a capital
    if word =~ '[A-Z]\w*\.\([A-Z]\w*\.\)*\w*'
        " cursor on a namespace
        let s:last_namespace = word
    else
        " not over namespace; re-browse last namespace we looked at
        let word = s:last_namespace
    endif
    call s:IdrisCmd(s:InIdris2, "browse-namespace", word, s:print_response)
  endif
endfunction
" }}}
" {{{ Replace word
function! s:ReplaceWordResponse(req, response)
    let text = a:response[0]
    execute "normal ciW" . printf("%s", text)
endfunction

function! IdrisProofSearch(hint)
    if IdrisReloadGuard(function("IdrisProofSearch", [a:hint]))
        let cline = line(".")
        let word = s:currentQueryObject()

        if (a:hint==0)
            let hints = []
        else
            let hints = split(input ("Hints: "), ',')
        endif
        call s:IdrisCmd(s:InAnyIdris, "proof-search", cline, word, hints, s:mkGeneric("s:ReplaceWordResponse"))
    endif
endfunction
"}}}
" {{{ Lemmas
function! s:LemmaResponse(req, response)
    let typeDec = a:response[0]
    let useSite = a:response[1]
    execute 'normal ciW' . printf('%s', useSite)
    execute 'normal {Oo' . printf('%s', typeDec)
endfunction

function! IdrisMakeLemma()
    if IdrisReloadGuard(function("IdrisMakeLemma"))
        let cline = line(".")
        let word = s:currentQueryObject()
        call s:IdrisCmd(s:InAnyIdris, "make-lemma", cline, word, s:mkGeneric("s:LemmaResponse"))
    endif
endfunction

function! s:SmallLemmaResponse(req, response)
    let typeDec = a:response[-1]
    execute 'normal F?x'
    execute 'normal {Oo' . printf('%s', typeDec)
endfunction

function! IdrisMakeSmallLemma()
    if IdrisReloadGuard(function("IdrisMakeSmallLemma"))
        let cline = line(".")
        let word = s:currentQueryObject()
        call s:IdrisCmd(s:InAnyIdris, "type-of", word, s:mkGeneric("s:SmallLemmaResponse"))
    endif
endfunction
" }}}
" {{{ unimplemented
" function! IdrisRefine()
"   let view = winsaveview()
"   w
"   let cline = line(".")
"   let word = expand("<cword>")
"   let tc = IdrisReload(1)
" 
"   let name = input ("Name: ")
" 
"   if (tc is "")
"     let result = s:IdrisCommand(":ref!", cline, word, name)
"     if (! (result is ""))
"        call IWrite(result)
"     else
"       e
"       call winrestview(view)
"     endif
"   endif
" endfunction
" 
" function! IdrisAddMissing()
"   let view = winsaveview()
"   w
"   let cline = line(".")
"   let word = expand("<cword>")
"   let tc = IdrisReload(1)
" 
"   if (tc is "")
"     let result = s:IdrisCommand(":am!", cline, word)
"     if (! (result is ""))
"        call IWrite(result)
"     else
"       e
"       call winrestview(view)
"     endif
"   endif
" endfunction
" }}}
" {{{ *replace line
function! s:ReplaceLine(req, response)
    call append(a:req.cline, a:response)
    execute a:req.cline . 'delete'
    call search(a:req.cursor_on)
endfunction

function s:ReplaceLineCmd(command, cursor_on)
    let cline = line('.')
    let word = s:currentQueryObject()
    let req = s:mkGeneric("s:ReplaceLine", {'cline':cline, 'cursor_on':(a:cursor_on) })
    call s:IdrisCmd(s:InAnyIdris, a:command, cline, word, req)
endfunction

function! IdrisCaseSplit()
  if IdrisReloadGuard(function("IdrisCaseSplit"))
      call s:ReplaceLineCmd("case-split", "?")
  endif
endfunction

function! IdrisMakeWith()
  if IdrisReloadGuard(function("IdrisMakeWith"))
      call s:ReplaceLineCmd("make-with", "_")
  endif
endfunction

function! IdrisMakeCase()
  if IdrisReloadGuard(function("IdrisMakeCase"))
      call s:ReplaceLineCmd("make-case", "_")
  endif
endfunction
" }}}
" {{{ *insert lines
function! s:AddClauseResponse(req, response)
    normal }b
    call append(line('.'), a:response)
    call search("?")
endfunction

function! IdrisAddClause(proof)
  if IdrisReloadGuard(function("IdrisAddClause", [a:proof]))
    let cline = line(".")
    let word = expand("<cword>")
    if a:proof != 0 && s:protocol_version == 1
        let command = "add-proof-clause"
    else
        let command = "add-clause"
    endif
    call s:IdrisCmd(s:InAnyIdris, command, cline, word, s:mkGeneric("s:AddClauseResponse", {'cline':cline}))
  endif
endfunction

function! s:GenDefResponse(req, response)
    normal }b
    call append(line('.'), a:response)
    normal }
endfunction

function! IdrisGenerateDef()
  if IdrisReloadGuard(function("IdrisGenerateDef"))
    let cline = line(".")
    let word = expand("<cword>")
    call s:IdrisCmd(s:InAnyIdris, "generate-def", cline, word, s:mkGeneric("s:GenDefResponse", {'cline':cline}))
  endif
endfunction
" }}}
" {{{ Eval
function! s:EvalResponse(req, command)
    let text = a:command[0]
    call IWrite(a:req.expr . ' = ')
    call IAppend(text)
endfunction

function! IdrisEval()
  if IdrisReloadGuard(function("IdrisEval"))
     let expr = input ("Expression: ")
      call s:IdrisCmd(s:InAnyIdris, "interpret", expr, s:mkGeneric("s:EvalResponse", {'expr':expr}))
      "TODO : Prints `it`, but not `stdout`
  endif
endfunction
" }}}
" }}}
" }}}

" {{{ Key maps
nnoremap <buffer> <silent> <LocalLeader>t :call IdrisShowType()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>r :call IdrisReload(0)<ENTER>
nnoremap <buffer> <silent> <LocalLeader>c :call IdrisCaseSplit()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>d 0:call search(":")<ENTER>b:call IdrisAddClause(0)<ENTER>
nnoremap <buffer> <silent> <LocalLeader>g :call IdrisGenerateDef()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>b 0:call IdrisAddClause(0)<ENTER>
nnoremap <buffer> <silent> <LocalLeader>m :call IdrisAddMissing()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>md 0:call search(":")<ENTER>b:call IdrisAddClause(1)<ENTER>
nnoremap <buffer> <silent> <LocalLeader>f :call IdrisRefine()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>o :call IdrisProofSearch(0)<ENTER>
nnoremap <buffer> <silent> <LocalLeader>p :call IdrisProofSearch(1)<ENTER>
nnoremap <buffer> <silent> <LocalLeader>l :call IdrisMakeLemma()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>e :call IdrisEval()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>w 0:call IdrisMakeWith()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>mc :call IdrisMakeCase()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>i 0:call IdrisResponseWin()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>h :call IdrisShowDoc()<ENTER>
nnoremap <buffer> <silent> <LocalLeader>n :call IdrisBrowseNamespace()<ENTER>
" Missing in engine
" nnoremap <buffer> <silent> <LocalLeader>d :call IdrisPrintDef()<ENTER>

menu Idris.Reload <LocalLeader>r
menu Idris.Show\ Type <LocalLeader>t
menu Idris.Evaluate <LocalLeader>e
menu Idris.-SEP0- :
menu Idris.Add\ Clause <LocalLeader>d
menu Idris.Add\ with <LocalLeader>w
menu Idris.Case\ Split <LocalLeader>c
menu Idris.Add\ missing\ cases <LocalLeader>m
menu Idris.Proof\ Search <LocalLeader>o
menu Idris.Proof\ Search\ with\ hints <LocalLeader>p

" }}}

" {{{ Autocommands

au BufHidden idris-response call IdrisHideResponseWin()
au BufEnter idris-response call IdrisShowResponseWin()
au BufWrite *.idr call s:IdrisMustReload()
" }}}
