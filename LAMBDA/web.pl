:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(http/http_parameters)).
:- use_module(library(memfile)).

:- http_handler(root(.),	get_gui, []).
:- http_handler(root(upload),	upload,      []).
:- http_handler(root(run),	 run,      []).
:- http_handler(root(add),	 add,      []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).
        
get_gui(_Request) :-  get_template(X),
	format('Content-type: text/html~n~n'),
	format(X, ["","",""]).
	
upload(Request) :-
	multipart_post_request(Request), !,
	http_read_data(Request, [file=Data], []),
	%load_file_content(Data),
	get_template(X),
	format('Content-type: text/html~n~n'),
	format(X, [Data,Data,'']).

upload(_Request) :-
	throw(http_reply(bad_request(bad_file_upload))).
	
	
add(Request) :-  
    member(method(post), Request), !,
      http_read_data(Request,
                        [editor=Data],[]),
      load_file_content(Data),
      get_template(X),
	format('Content-type: text/html~n~n'),
	format(X, [Data,Data,'Adding to database']).
	
	
run(Request) :-  
    member(method(post), Request), !,
      http_read_data(Request,
                       [strategy=Stat,command=Cmd,content=Cnt,data=Data],[]),
      set_reduction(Stat),
      string_concat(Cmd," ",Temp),
      string_concat(Temp,Cnt,In),
      tmp_file('txt',Name),
      tell(Name),go3(In),told, 
      open(Name,read,Stream1),
      read_string(Stream1,_,Output), close(Stream1),
      get_template(X),
      format('Content-type: text/html~n~n'),
      format(X, [Data,Data,Output]).

	
multipart_post_request(Request) :-
	memberchk(method(post), Request),
	memberchk(content_type(ContentType), Request),
	http_parse_header_value(
    content_type, ContentType,
    media(multipart/'form-data', _)).

:- public save_file/3.

save_file(In, file(FileName, File), Options) :-
   option(filename(FileName), Options),
   setup_call_cleanup(
   tmp_file_stream(octet, File, Out),
   copy_stream_data(In, Out),
   close(Out)).
   
get_template(X) :- open('index1.html','read', Stream),
                                    read_string(Stream,_,X),close(Stream).
                                    

   
