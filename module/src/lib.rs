extern crate libcereal;
#[macro_use] extern crate emacs_module_bindings as emacs;
extern crate libc;

use emacs::{ConvResult, EmacsEnv, EmacsRT, EmacsVal};
use emacs::elisp2native as e2n;
use emacs::native2elisp as n2e;
use libcereal::*;
use libcereal::amplify::*;
use std::os::raw;

/// This states that the module is GPL-compliant.
/// Emacs won't load the module if this symbol is undefined.
#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;


init_module! { (env) {
    /************************** UClient **************************/
    emacs::register(env, "cereal/uclient-new", Fuclient_new,      0..0,
                    "()\n\n\
                     Create a new unconnected client.")?;

    emacs::register(env, "cereal/uclient-serialize-using-capnp", Fuclient_serialize_using_capnp,    1..1,
                    "(uclient)\n\n\
                     Use Capn Proto for serialization.")?;

    emacs::register(env, "cereal/uclient-serialize-using-json", Fuclient_serialize_using_json,      1..1,
                    "(uclient)\n\n\
                     Use JSON for serialization.")?;

    emacs::register(env, "cereal/uclient-receive-address", Fuclient_rx_addr,      2..2,
                    "(uclient address)\n\n\
                     Set the receive address.")?;

    emacs::register(env, "cereal/uclient-send-address", Fuclient_tx_addr,      2..2,
                    "(uclient address)\n\n\
                     Set the send address.")?;

    emacs::register(env, "cereal/uclient-connect", Fuclient_connect,      1..1,
                    "(uclient)\n\n\
                     Connect a uclient, and return a cclient instead.")?;


    /************************** CClient **************************/
    emacs::register(env, "cereal/cclient-send", Fcclient_send,      2..2,
                    "(cclient msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/cclient-receive", Fcclient_receive,      2..2,
                    "(cclient msg)\n\n\
                     .")?;


    /************************** Msg **************************/
    emacs::register(env, "cereal/msg-new", Fmsg_new,      0..0,
                    "()\n\n\
                     Create a new message.")?;

    emacs::register(env, "cereal/msg-set-source", Fmsg_set_source,      2..2,
                    "(msg source)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-source", Fmsg_get_source,      1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-set-request-number", Fmsg_set_request_number,      2..2,
                    "(msg request-number)\n\n\
                     .")?;

    emacs::register(env, "cereal/msg-get-request-number", Fmsg_get_request_number,      1..1,
                    "(msg)\n\n\
                     .")?;

    // emacs::register(env, "cereal/msg-", Fmsg_,      2..2,
    //                 "()\n\n\
    //                  .")?;



    /************************** Contents **************************/
    // emacs::register(env, "cereal/contents-new", Fcontents_new,      0..0,
    //                 "()\n\n\
    //                  Create a new Contents object.")?;

    /************************** Region **************************/
    // emacs::register(env, "cereal/region-new", Fregion_new,      0..0,
    //                 "()\n\n\
    //                  Create a new Region object.")?;

    /************************** Language  **************************/
    // emacs::register(env, "cereal/language-new", Flanguage_new,      0..0,
    //                 "()\n\n\
    //                  Create a new Language object.")?;

    /************************** Ast **************************/
    // emacs::register(env, "cereal/ast-new", Fast_new,      0..0,
    //                 "()\n\n\
    //                  Create a new Ast object.")?;


    const MODULE_NAME: &str = "libcereal-module";
    emacs::provide(env, MODULE_NAME.to_string());
    message!(env, "[CEREAL] {} initialized", MODULE_NAME)
}}


emacs_subrs! {
    // Create a GC'd Elisp handle to a CClient value
    Fuclient_new(env, nargs, args, data, TAG) {
        let client = UClient::new().unwrap(/* TODO: ClientErr */);
        n2e::boxed(env, client, emacs::destruct::<UClient>)
    };

    Fuclient_serialize_using_capnp(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        uclient.set_serialization_method(libcereal::Method::CapnProto);
        n2e::symbol(env, "nil")
    };

    Fuclient_serialize_using_json(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        uclient.set_serialization_method(libcereal::Method::Json);
        n2e::symbol(env, "nil")
    };

    Fuclient_rx_addr(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let addr = e2n::string(env, *args.offset(1)).unwrap(/* TODO: ConvErr */);
        let addr = Url::parse(addr.as_str()).unwrap(/* TODO: url ParseError */);
        uclient.set_receive_addr(addr);
        n2e::symbol(env, "nil")
    };

    Fuclient_tx_addr(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        let addr = e2n::string(env, *args.offset(1)).unwrap(/* TODO: ConvErr */);
        let addr = Url::parse(addr.as_str()).unwrap(/* TODO: url ParseError */);
        uclient.set_send_addr(addr);
        n2e::symbol(env, "nil")
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



    Fcclient_send(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let msg: &Msg = e2n::mut_ref(env, args, 1).unwrap(/* TODO: ConvErr */);
        cclient.send(msg).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "nil")
    };

    Fcclient_receive(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let msg: &mut Msg = e2n::mut_ref(env, args, 1).unwrap(/* TODO: ConvErr */);
        cclient.receive(msg).unwrap(/* TODO: ClientErr */);
        n2e::symbol(env, "nil")
    };



    Fmsg_new(env, nargs, args, data, TAG) {
        n2e::boxed(env, Msg::default(), emacs::destruct::<Msg>)
    };

    Fmsg_set_source(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        *msg.source_mut() = e2n::string(env, *args.offset(1)).unwrap(/* TODO: ConvErr */);
        n2e::symbol(env, "nil")
    };

    Fmsg_get_source(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        let source: &str = msg.source_ref();
        n2e::string(env, source)
    };

    Fmsg_set_request_number(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let reqno: i64 = e2n::integer(env, args, 1).unwrap(/* TODO: ConvErr */);
        if reqno < 0 {
            // TODO: error: reqno >= 0 doesn't hold
        }
        *msg.request_number_mut() = reqno as u64;
        n2e::symbol(env, "nil")
    };

    Fmsg_get_request_number(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        n2e::integer(env, msg.request_number() as i64)
    };

    // Fmsg_(env, nargs, args, data, TAG) {
    //     let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
    // };

    // Fmsg_(env, nargs, args, data, TAG) {
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
//  LocalWords:  ast
