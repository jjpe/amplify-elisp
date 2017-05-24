extern crate libcereal;
#[macro_use] extern crate emacs_module_bindings as emacs;
extern crate libc;

use emacs::{ConvResult, EmacsEnv, EmacsRT, EmacsVal};
use emacs::elisp2native as e2n;
use emacs::native2elisp as n2e;
use libcereal::ZmqErr;
use libcereal::*;
use libcereal::amplify::*;
use std::ffi::CString;
use std::os::raw;

/// This states that the module is GPL-compliant.
/// Emacs won't load the module if this symbol is undefined.
#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;


init_module! { (env) {
    /************************** UClient **************************/
    emacs::register(env, "cereal/uclient-new",
                    Fuclient_new,  0..0,
                    "()\n\n\
                     Create a new unconnected client.")?;

    emacs::register(env, "cereal/uclient-serialize-using-capnp",
                    Fuclient_serialize_using_capnp,  1..1,
                    "(uclient)\n\n\
                     Use Capn Proto for serialization.")?;

    emacs::register(env, "cereal/uclient-serialize-using-json",
                    Fuclient_serialize_using_json,  1..1,
                    "(uclient)\n\n\
                     Use JSON for serialization.")?;

    emacs::register(env, "cereal/uclient-set-receive-address",
                    Fuclient_set_rx_addr,  2..2,
                    "(uclient address)\n\n\
                     Set the receive address.")?;

    emacs::register(env, "cereal/uclient-set-send-address",
                    Fuclient_set_tx_addr,  2..2,
                    "(uclient address)\n\n\
                     Set the send address.")?;

    emacs::register(env, "cereal/uclient-set-receive-timeout",
                    Fuclient_set_rx_timeout,  2..2,
                    "(uclient timeout-millis)\n\n\
                     Set the receive timeout, in milliseconds.")?;

    emacs::register(env, "cereal/uclient-set-send-timeout",
                    Fuclient_set_tx_timeout,  2..2,
                    "(uclient timeout-millis)\n\n\
                     Set the send timeout, in milliseconds.")?;

    emacs::register(env, "cereal/uclient-set-receive-hwm",
                    Fuclient_set_rx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the receive high water mark capacity.")?;

    emacs::register(env, "cereal/uclient-set-send-hwm",
                    Fuclient_set_tx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the send high water mark capacity.")?;

    emacs::register(env, "cereal/uclient-connect",
                    Fuclient_connect,  1..1,
                    "(uclient)\n\n\
                     Connect a uclient, and return a cclient instead.")?;


    /************************** CClient **************************/
    emacs::register(env, "cereal/cclient-set-receive-timeout",
                    Fcclient_set_rx_timeout,  2..2,
                    "(cclient timeout-millis)\n\n\
                     Set the receive timeout, in milliseconds.")?;

    emacs::register(env, "cereal/cclient-set-send-timeout",
                    Fcclient_set_tx_timeout,  2..2,
                    "(cclient timeout-millis)\n\n\
                     Set the send timeout, in milliseconds.")?;

    emacs::register(env, "cereal/cclient-set-receive-hwm",
                    Fcclient_set_rx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the receive high water mark capacity.")?;

    emacs::register(env, "cereal/cclient-set-send-hwm",
                    Fcclient_set_tx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the send high water mark capacity.")?;

    emacs::register(env, "cereal/cclient-send",
                    Fcclient_send,  2..2,
                    "(cclient msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/cclient-receive",
                    Fcclient_receive,  2..2,
                    "(cclient msg)\n\n\
                     .")?;


    /************************** Msg **************************/
    emacs::register(env, "cereal/msg-new",
                    Fmsg_new,  0..0,
                    "()\n\n\
                     Create a new message.")?;

    emacs::register(env, "cereal/msg-set-source",
                    Fmsg_set_source,  2..2,
                    "(msg source)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-source",
                    Fmsg_get_source,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-set-request-number",
                    Fmsg_set_request_number,  2..2,
                    "(msg request-number)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-request-number",
                    Fmsg_get_request_number,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-set-origin",
                    Fmsg_set_origin,  2..2,
                    "(msg origin)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-origin",
                    Fmsg_get_origin,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-set-contents",
                    Fmsg_set_contents,  2..2,
                    "(msg contents)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-contents",
                    Fmsg_get_contents,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-add-region",
                    Fmsg_add_region,  2..2,
                    "(msg region)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-clear-regions",
                    Fmsg_clear_regions,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-region",
                    Fmsg_get_region,  2..2,
                    "(msg index)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-count-regions",
                    Fmsg_count_regions,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-language",
                    Fmsg_get_language,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-set-language",
                    Fmsg_set_language,  2..2,
                    "(msg language)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-set-ast",
                    Fmsg_set_ast,  2..2,
                    "(msg ast)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-ast",
                    Fmsg_get_ast,  1..1,
                    "(msg)\n\n\
                     .")?;


    /************************** Contents **************************/
    emacs::register(env, "cereal/contents-new-text",
                    Fcontents_new_text,  1..1,
                    "(text)\n\n\
                     Create a new Contents::Text object.")?;

    emacs::register(env, "cereal/contents-new-entries",
                    Fcontents_new_entries,  0..1000,
                    "()\n\n\
                     Create a new Contents::Entries object.")?;

    emacs::register(env, "cereal/contents-is-text",
                    Fcontents_is_text,  1..1,
                    "(contents)\n\n\
                     Return t iff. contents is Contents::Text, otherwise nil.")?;

    emacs::register(env, "cereal/contents-add-text",
                    Fcontents_add_text,  2..2,
                    "(contents text)\n\n\
                     .")?;

    emacs::register(env, "cereal/contents-get-text",
                    Fcontents_get_text,  1..1,
                    "(contents)\n\n\
                     .")?;

    emacs::register(env, "cereal/contents-is-entries",
                    Fcontents_is_entries,  1..1,
                    "(contents)\n\n\
                     Return t iff. contents is Contents::Entries, otherwise nil.")?;

    emacs::register(env, "cereal/contents-add-entry",
                    Fcontents_add_entry,  2..2,
                    "(contents entry)\n\n\
                     .")?;

    emacs::register(env, "cereal/contents-get-entry",
                    Fcontents_get_entry,  2..2,
                    "(contents index)\n\n\
                     .")?;

    emacs::register(env, "cereal/contents-count-entries",
                    Fcontents_count_entries,  1..1,
                    "(msg)\n\n\
                     .")?;


    /************************** Region **************************/
    emacs::register(env, "cereal/region-new",
                    Fregion_new,  2..2,
                    "(begin end)\n\n\
                     Create a new Region object.")?;

    emacs::register(env, "cereal/region-get-begin",
                    Fregion_get_begin,  1..1,
                    "(region)\n\n\
                     .")?;

    emacs::register(env, "cereal/region-get-end",
                    Fregion_get_end,  1..1,
                    "(region)\n\n\
                     .")?;


    /************************** Language  **************************/
    emacs::register(env, "cereal/language-new",
                    Flanguage_new,  1..1,
                    "(name)\n\n\
                     Create a new Language object.")?;

    emacs::register(env, "cereal/language-set-name",
                    Flanguage_set_name,  2..2,
                    "(language name)\n\n\
                     .")?;

    emacs::register(env, "cereal/language-get-name",
                    Flanguage_get_name,  1..1,
                    "(language)\n\n\
                     .")?;


    /************************** Ast **************************/
    emacs::register(env, "cereal/ast-new",
                    Fast_new,  1..1,
                    "(name)\n\n\
                     Create a new Ast object.")?;

    emacs::register(env, "cereal/ast-get-name",
                    Fast_get_name,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-set-data",
                    Fast_set_data,  2..2,
                    "(ast data)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-get-data",
                    Fast_get_data,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-clear-data",
                    Fast_clear_data,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-add-child",
                    Fast_add_child,  2..2,
                    "(ast child)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-get-child",
                    Fast_get_child,  2..2,
                    "(ast index)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-clear-children",
                    Fast_clear_children,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "cereal/ast-count-children",
                    Fast_count_children,  1..1,
                    "(ast)\n\n\
                     .")?;


    const MODULE_NAME: &str = "libcereal-module";
    emacs::provide(env, MODULE_NAME.to_string());
    message!(env, "[cereal] {} initialized", MODULE_NAME)
}}


emacs_subrs! {
    /************************** UClient **************************/
    // Create a GC'd Elisp handle to a CClient value
    Fuclient_new(env, nargs, args, data, TAG) {
        let client = UClient::new().unwrap(/* TODO: ClientErr */);
        n2e::boxed(env, client, emacs::destruct::<UClient>)
    };

    Fuclient_serialize_using_capnp(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        uclient.set_serialization_method(libcereal::Method::CapnProto);
        n2e::symbol(env, "t")
    };

    Fuclient_serialize_using_json(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        uclient.set_serialization_method(libcereal::Method::Json);
        n2e::symbol(env, "t")
    };

    Fuclient_set_rx_addr(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let addr = e2n::string(env, *args.offset(1))?;
        let addr = Url::parse(addr.as_str()).unwrap(/* TODO: url ParseError */);
        uclient.set_receive_addr(addr);
        n2e::symbol(env, "t")
    };

    Fuclient_set_tx_addr(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let addr = e2n::string(env, *args.offset(1))?;
        let addr = Url::parse(addr.as_str()).unwrap(/* TODO: url ParseError */);
        uclient.set_send_addr(addr);
        n2e::symbol(env, "t")
    };

    Fuclient_set_rx_timeout(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let timeout = Timeout::from_number(e2n::integer(env, args, 1)? as isize);
        uclient.set_receive_timeout(timeout);
        n2e::symbol(env, "t")
    };

    Fuclient_set_tx_timeout(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let timeout = Timeout::from_number(e2n::integer(env, args, 1)? as isize);
        uclient.set_send_timeout(timeout);
        n2e::symbol(env, "t")
    };

    Fuclient_set_rx_hwm(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let hwm = Hwm::from_number(e2n::integer(env, args, 1)? as usize);
        uclient.set_receive_hwm(hwm);
        n2e::symbol(env, "t")
    };

    Fuclient_set_tx_hwm(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let hwm = Hwm::from_number(e2n::integer(env, args, 1)? as usize);
        uclient.set_send_hwm(hwm);
        n2e::symbol(env, "t")
    };

    Fuclient_connect(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let cclient: CClient = uclient
            .clone(
                /* TODO: The proper solution is being able to take ownership */
                /*       of a pointer back from Elisp. Then this clone won't */
                /*       be necessary anymore.                               */
            )
            .connect().unwrap(/* TODO: ClientErr */);
        n2e::boxed(env, cclient, emacs::destruct::<CClient>)
    };


    /************************** CClient **************************/
    Fcclient_set_rx_timeout(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let timeout = Timeout::from_number(e2n::integer(env, args, 1)? as isize);
        cclient.set_receive_timeout(timeout).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "t")
    };

    Fcclient_set_tx_timeout(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let timeout = Timeout::from_number(e2n::integer(env, args, 1)? as isize);
        cclient.set_send_timeout(timeout).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "t")
    };

    Fcclient_set_rx_hwm(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let hwm = Hwm::from_number(e2n::integer(env, args, 1)? as usize);
        cclient.set_receive_hwm(hwm).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "t")
    };

    Fcclient_set_tx_hwm(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let hwm = Hwm::from_number(e2n::integer(env, args, 1)? as usize);
        cclient.set_send_hwm(hwm).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "t")
    };

    Fcclient_send(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let msg: &Msg = e2n::mut_ref(env, args, 1)?;
        cclient.send(msg).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "nil")
    };

    Fcclient_receive(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let msg: &mut Msg = e2n::mut_ref(env, args, 1)?;
        match cclient.receive(msg) {
            Ok(()) => n2e::symbol(env, "t"),
            Err(ClientErr::FailedToReceive(ZmqErr::EINTR)) =>
            {
                // println!("[Fcclient_receive] got Err(ClientErr::FailedToReceive(ZmqErr::EINTR))");
                n2e::symbol(env, ":interrupted")}, // Interrupt
            Err(ClientErr::FailedToReceive(ZmqErr::EAGAIN)) =>
            {
                // println!("[Fcclient_receive] got Err(ClientErr::FailedToReceive(ZmqErr::EAGAIN))");
                n2e::symbol(env, ":no-msg")}, // No msg at the moment
            Err(ClientErr::FailedToReceive(zmqerr)) => {
                // println!("Before I crash, here's the zmq err: {:#?}", zmqerr);
                panic!("zmqerr: {:?}", zmqerr)
            }
            client_err => panic!("{:?}", client_err), // TODO:
            // Err(client_err) => Err(client_err),
        }
    };


    /************************** Msg **************************/
    Fmsg_new(env, nargs, args, data, TAG) {
        n2e::boxed(env, Msg::default(), emacs::destruct::<Msg>)
    };

    Fmsg_set_source(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let cstring: CString = e2n::cstring(env, *args.offset(1))?;
        *msg.source_mut() = cstring.into_string()?;
        n2e::symbol(env, "t")
    };

    Fmsg_get_source(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        let source: &str = msg.source_ref();
        n2e::string(env, source)
    };

    Fmsg_set_request_number(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let reqno: i64 = e2n::integer(env, args, 1)?;
        if reqno < 0 {
            // TODO: error: reqno >= 0 doesn't hold
        }
        *msg.request_number_mut() = reqno as u64;
        n2e::symbol(env, "t")
    };

    Fmsg_get_request_number(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        n2e::integer(env, msg.request_number() as i64)
    };

    Fmsg_set_origin(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let origin: EmacsVal = *args.offset(1);
        if emacs::eq(env, origin, n2e::symbol(env, "nil")?)? {
            *msg.origin_mut() = None;
            return n2e::symbol(env, "t");
        }
        let cstring: CString = e2n::cstring(env, origin)?;
        *msg.origin_mut() = Some(cstring.into_string()?);
        n2e::symbol(env, "t")
    };

    Fmsg_get_origin(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        match msg.origin_ref() {
            None => n2e::symbol(env, "nil"),
            Some(origin) => n2e::string(env, origin),
        }
    };

    Fmsg_set_contents(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        if emacs::eq(env, *args.offset(1), n2e::symbol(env, "nil")?)? {
            *msg.contents_mut() = None;
            return n2e::symbol(env, "t");
        }
        let contents: &Contents = e2n::mut_ref(env, args, 1)?;
        *msg.contents_mut() = Some(contents.clone(/* TODO: remove the clone */));
        n2e::symbol(env, "t")
    };

    Fmsg_get_contents(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        match msg.contents_ref() {
            None => n2e::symbol(env, "nil"),
            Some(&Contents::Text(ref t)) => n2e::string(env, t.as_bytes()),
            Some(&Contents::Entries(ref es)) => n2e::string_list(env, es),
        }
    };

    Fmsg_add_region(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let region: &Region = e2n::mut_ref(env, args, 1)?;
        msg.regions_mut().push(region.clone(/* TODO: remove clone() call */));
        n2e::symbol(env, "t")
    };

    Fmsg_clear_regions(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        msg.regions_mut().clear();
        n2e::symbol(env, "t")
    };

    Fmsg_get_region(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let index: i64 = e2n::integer(env, args, 1)?;
        if index < 0 {
            // TODO: error: index must not be negative
        }
        let index: u64 = index as u64;
        let num_regions: u64 = msg.regions_ref().len() as u64;
        if index >= num_regions {
            // TODO: error: index must be smaller than num_regions
        }
        let region: Region = msg.regions_ref()[index as usize];
        n2e::boxed(env, region, emacs::destruct::<Region>)
    };

    Fmsg_count_regions(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let num_regions: i64 = msg.regions_ref().len() as i64;
        n2e::integer(env, num_regions)
    };


    Fmsg_get_language(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let lang_ref: Option<&Language> = msg.language_ref();
        let language: Language = lang_ref.map(|lang: &Language| lang.clone())
            .unwrap_or(Language::from(""));
        n2e::boxed(env, language, emacs::destruct::<Language>)
    };

    Fmsg_set_language(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        if emacs::eq(env, *args.offset(1), n2e::symbol(env, "nil")?)? {
            *msg.language_mut() = None;
            return n2e::symbol(env, "t");
        }
        let language: &Language = e2n::mut_ref(env, args, 1)?;
        *msg.language_mut() = Some(language.clone(/* TODO: remove the clone */));
        n2e::symbol(env, "t")
    };

    Fmsg_get_ast(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        match msg.ast_ref() as Option<&Ast> {
            None => n2e::symbol(env, "nil"),
            Some(ast) => {
                let ast: Ast = ast.clone(/* TODO: ability to take ownership */);
                n2e::boxed(env, ast, emacs::destruct::<Ast>)
            },
        }
    };

    Fmsg_set_ast(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        if emacs::eq(env, *args.offset(1), n2e::symbol(env, "nil")?)? {
            *msg.ast_mut() = None;
            return n2e::symbol(env, "t");
        }
        let ast: &Ast = e2n::mut_ref(env, args, 1)?;
        *msg.ast_mut() = Some(ast.clone(/* TODO: remove the clone */));
        n2e::symbol(env, "t")
    };


    /************************** Contents **************************/
    Fcontents_new_text(env, nargs, args, data, TAG) {
        let text: String = e2n::string(env, *args.offset(0))?;
        n2e::boxed(env, Contents::Text(text), emacs::destruct::<Contents>)
    };

    Fcontents_new_entries(env, nargs, args, data, TAG) {
        let mut strings: Vec<String> = vec![];
        for idx in 0 .. nargs {
            strings.push(e2n::string(env, *args.offset(idx))?);
        }
        n2e::boxed(env, Contents::Entries(strings), emacs::destruct::<Contents>)
    };

    Fcontents_is_text(env, nargs, args, data, TAG) {
        let contents: &Contents = e2n::mut_ref(env, args, 0)?;
        match contents {
            &Contents::Text(_) => n2e::symbol(env, "t"),
            _ => n2e::symbol(env, "nil"),
        }
    };

    Fcontents_add_text(env, nargs, args, data, TAG) {
        let contents: &mut Contents = e2n::mut_ref(env, args, 0)?;
        let string: String = e2n::string(env, *args.offset(1))?;
        match contents {
            &mut Contents::Text(ref mut text) => {
                text.push_str(string.as_str());
                n2e::symbol(env, "t")
            },
            _ => n2e::symbol(env, ":error::not-text"),
        }
    };

    Fcontents_get_text(env, nargs, args, data, TAG) {
        let contents: &Contents = e2n::mut_ref(env, args, 0)?;
        match contents {
            &Contents::Text(ref text) => n2e::string(env, text.as_str()),
            _ => n2e::symbol(env, ":error::not-text"),
        }
    };

    Fcontents_is_entries(env, nargs, args, data, TAG) {
        let contents: &Contents = e2n::mut_ref(env, args, 0)?;
        match contents {
            &Contents::Entries(_) => n2e::symbol(env, "t"),
            _ => n2e::symbol(env, "nil"),
        }
    };

    Fcontents_add_entry(env, nargs, args, data, TAG) {
        let contents: &mut Contents = e2n::mut_ref(env, args, 0)?;
        let string: String = e2n::string(env, *args.offset(1))?;
        match contents {
            &mut Contents::Entries(ref mut entries) => {
                entries.push(string);
                n2e::symbol(env, "t")
            },
            _ => n2e::symbol(env, ":error::not-entries"),
        }
    };

    Fcontents_get_entry(env, nargs, args, data, TAG) {
        let contents: &Contents = e2n::mut_ref(env, args, 0)?;
        let index: i64 = e2n::integer(env, args, 1)?;
        if index < 0 {
            // TODO: error
        }
        match contents {
            &Contents::Entries(ref entries) => {
                if index as usize >= entries.len() {
                    // TODO: error
                }
                let entry: &String = entries.get(index as usize)
                    .unwrap(/* TODO: None */);
                n2e::string(env, entry.as_str())
            },
            _ => n2e::symbol(env, ":error::not-entries"),
        }
    };

    Fcontents_count_entries(env, nargs, args, data, TAG) {
        let contents: &Contents = e2n::mut_ref(env, args, 0)?;
        match contents {
            &Contents::Entries(ref entries) =>
                n2e::integer(env, entries.len() as i64),
            _ => n2e::symbol(env, ":error::not-entries"),
        }
    };



    /************************** Region **************************/
    Fregion_new(env, nargs, args, data, TAG) {
        let begin: i64 = e2n::int_value(env, *args.offset(0))?;
        let end: i64 = e2n::int_value(env, *args.offset(1))?;
        if begin < 0 {
            // TODO: error: begin must not be negative
        }
        if end < 0 {
            // TODO: Error: end must not be negative
        }
        if end < begin {
            // TODO: Error: end must not be larger than begin
        }
        let region = Region { begin: begin as u64, end: end as u64 };
        n2e::boxed(env, region, emacs::destruct::<Region>)
    };

    Fregion_get_begin(env, nargs, args, data, TAG) {
        let region: &Region = e2n::mut_ref(env, args, 0)?;
        n2e::integer(env, region.begin as i64)
    };

    Fregion_get_end(env, nargs, args, data, TAG) {
        let region: &Region = e2n::mut_ref(env, args, 0)?;
        n2e::integer(env, region.end as i64)
    };


    /************************** Language **************************/
    Flanguage_new(env, nargs, args, data, TAG) {
        let string: String = e2n::string(env, *args.offset(0))?;
        n2e::boxed(env, Language::from(string), emacs::destruct::<Language>)
    };

    Flanguage_get_name(env, nargs, args, data, TAG) {
        let language: &Language = e2n::mut_ref(env, args, 0)?;
        let language: &str = language.as_ref();
        n2e::string(env, language)
    };

    Flanguage_set_name(env, nargs, args, data, TAG) {
        let language: &mut Language = e2n::mut_ref(env, args, 0)?;
        *language.as_mut() = e2n::string(env, *args.offset(1))?;
        n2e::symbol(env, "t")
    };


    /************************** Ast **************************/
    Fast_new(env, nargs, args, data, TAG) {
        let name: String = e2n::string(env, *args.offset(0))?;
        n2e::boxed(env, Ast::new(name), emacs::destruct::<Ast>)
    };

    Fast_get_name(env, nargs, args, data, TAG) {
        let ast: &Ast = e2n::mut_ref(env, args, 0)?;
        n2e::string(env, ast.name_ref())
    };

    Fast_set_data(env, nargs, args, data, TAG) {
        let ast: &mut Ast = e2n::mut_ref(env, args, 0)?;
        *ast.data_mut() = e2n::string(env, *args.offset(1))?;
        n2e::symbol(env, "t")
    };

    Fast_get_data(env, nargs, args, data, TAG) {
        let ast: &Ast = e2n::mut_ref(env, args, 0)?;
        n2e::string(env, ast.data_ref())
    };

    Fast_clear_data(env, nargs, args, data, TAG) {
        let ast: &mut Ast = e2n::mut_ref(env, args, 0)?;
        ast.data_mut().clear();
        n2e::symbol(env, "t")
    };

    Fast_add_child(env, nargs, args, data, TAG) {
        let ast: &mut Ast = e2n::mut_ref(env, args, 0)?;
        let child: &Ast = e2n::mut_ref(env, args, 1)?;
        let child: Ast = child.clone(/* TODO: remove the clone() call */);
        ast.children_mut().push(child);
        n2e::symbol(env, "t")
    };

    Fast_get_child(env, nargs, args, data, TAG) {
        let ast: &Ast = e2n::mut_ref(env, args, 0)?;
        let index: i64 = e2n::integer(env, args, 1)?;
        if index < 0 {
            // TODO: error
        }
        let child: Ast = ast.children_ref()[index as usize]
            .clone(/* TODO: remove the clone() call */);
        n2e::boxed(env, child, emacs::destruct::<Ast>)
    };

    Fast_clear_children(env, nargs, args, data, TAG) {
        let ast: &mut Ast = e2n::mut_ref(env, args, 0)?;
        ast.children_mut().clear();
        n2e::symbol(env, "t")
    };

    Fast_count_children(env, nargs, args, data, TAG) {
        let ast: &mut Ast = e2n::mut_ref(env, args, 0)?;
        let num_children = ast.children_ref().len();
        n2e::integer(env, num_children as i64)
    };

    // Fast_(env, nargs, args, data, TAG) {
    //     let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
    // };

}



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

//  LocalWords:  uclient cclient Fcereal capn Fmsg Fcclient Ast capnp
//  LocalWords:  ast stringp listp Fcontents aclient AsyncClient
