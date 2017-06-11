extern crate amplify;
#[macro_use] extern crate emacs_module_bindings as emacs;
extern crate libc;

use emacs::{ConvResult, EmacsEnv, EmacsRT, EmacsVal};
use emacs::elisp2native as e2n;
use emacs::native2elisp as n2e;
use amplify::*;
use amplify::amplify::*;
use std::ffi::CString;
use std::os::raw;

/// This states that the module is GPL-compliant.
/// Emacs won't load the module if this symbol is undefined.
#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;


init_module! { (env) {
    /************************** Ureporter **************************/
    emacs::register(env, "amplify-elisp/ureporter-new",
                    Fureporter_new,  0..0,
                    "()\n\n\
                     Create a new unconnected reporter.")?;

    emacs::register(env, "amplify-elisp/ureporter-serialize-using-capnp",
                    Fureporter_serialize_using_capnp,  1..1,
                    "(ureporter)\n\n\
                     Use Capn Proto for serialization.")?;

    emacs::register(env, "amplify-elisp/ureporter-serialize-using-json",
                    Fureporter_serialize_using_json,  1..1,
                    "(ureporter)\n\n\
                     Use JSON for serialization.")?;

    emacs::register(env, "amplify-elisp/ureporter-set-send-address",
                    Fureporter_set_tx_addr,  2..2,
                    "(ureporter address)\n\n\
                     Set the send address.")?;

    emacs::register(env, "amplify-elisp/ureporter-set-send-timeout",
                    Fureporter_set_tx_timeout,  2..2,
                    "(ureporter timeout-millis)\n\n\
                     Set the send timeout, in milliseconds.")?;

    emacs::register(env, "amplify-elisp/ureporter-set-send-hwm",
                    Fureporter_set_tx_hwm,  2..2,
                    "(ureporter capacity)\n\n\
                     Set the send high water mark capacity.")?;

    emacs::register(env, "amplify-elisp/ureporter-connect",
                    Fureporter_connect,  1..1,
                    "(ureporter)\n\n\
                     Consume the ureporter, and return a creporter instead.")?;

    /************************** CReporter **************************/
    emacs::register(env, "amplify-elisp/creporter-set-send-timeout",
                    Fcreporter_set_tx_timeout,  2..2,
                    "(creporter timeout-millis)\n\n\
                     Set the send timeout, in milliseconds.")?;

    emacs::register(env, "amplify-elisp/creporter-set-send-hwm",
                    Fcreporter_set_tx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the send high water mark capacity.")?;

    emacs::register(env, "amplify-elisp/creporter-send",
                    Fcreporter_send,  2..2,
                    "(creporter report)\n\n\
                     .")?;

    /************************** Report **************************/
    emacs::register(env, "amplify-elisp/report-new",
                    Freport_new,  0..0,
                    "()\n\n\
                     Create a new message.")?;

    emacs::register(env, "amplify-elisp/report-set-action",
                    Freport_set_action,  2..2,
                    "(report action)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-get-action",
                    Freport_get_action,  1..1,
                    "(report)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-set-process",
                    Freport_set_process,  2..2,
                    "(report process)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-get-process",
                    Freport_get_process,  1..1,
                    "(report)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-set-request-number",
                    Freport_set_request_number,  2..2,
                    "(report request-number)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-get-request-number",
                    Freport_get_request_number,  1..1,
                    "(report)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-set-duration-nanos",
                    Freport_set_duration_nanos,  2..2,
                    "(report duration-nanos)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-get-duration-nanos",
                    Freport_get_duration_nanos,  1..1,
                    "(report)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-set-command",
                    Freport_set_command,  2..2,
                    "(report command)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/report-get-command",
                    Freport_get_command,  1..1,
                    "(report)\n\n\
                     .")?;



    /************************** UClient **************************/
    emacs::register(env, "amplify-elisp/uclient-new",
                    Fuclient_new,  0..0,
                    "()\n\n\
                     Create a new unconnected client.")?;

    emacs::register(env, "amplify-elisp/uclient-serialize-using-capnp",
                    Fuclient_serialize_using_capnp,  1..1,
                    "(uclient)\n\n\
                     Use Capn Proto for serialization.")?;

    emacs::register(env, "amplify-elisp/uclient-serialize-using-json",
                    Fuclient_serialize_using_json,  1..1,
                    "(uclient)\n\n\
                     Use JSON for serialization.")?;

    emacs::register(env, "amplify-elisp/uclient-set-receive-address",
                    Fuclient_set_rx_addr,  2..2,
                    "(uclient address)\n\n\
                     Set the receive address.")?;

    emacs::register(env, "amplify-elisp/uclient-set-send-address",
                    Fuclient_set_tx_addr,  2..2,
                    "(uclient address)\n\n\
                     Set the send address.")?;

    emacs::register(env, "amplify-elisp/uclient-set-receive-timeout",
                    Fuclient_set_rx_timeout,  2..2,
                    "(uclient timeout-millis)\n\n\
                     Set the receive timeout, in milliseconds.")?;

    emacs::register(env, "amplify-elisp/uclient-set-send-timeout",
                    Fuclient_set_tx_timeout,  2..2,
                    "(uclient timeout-millis)\n\n\
                     Set the send timeout, in milliseconds.")?;

    emacs::register(env, "amplify-elisp/uclient-set-receive-hwm",
                    Fuclient_set_rx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the receive high water mark capacity.")?;

    emacs::register(env, "amplify-elisp/uclient-set-send-hwm",
                    Fuclient_set_tx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the send high water mark capacity.")?;

    emacs::register(env, "amplify-elisp/uclient-connect",
                    Fuclient_connect,  1..1,
                    "(uclient)\n\n\
                     Connect a uclient, and return a cclient instead.")?;


    /************************** CClient **************************/
    emacs::register(env, "amplify-elisp/cclient-set-receive-timeout",
                    Fcclient_set_rx_timeout,  2..2,
                    "(cclient timeout-millis)\n\n\
                     Set the receive timeout, in milliseconds.")?;

    emacs::register(env, "amplify-elisp/cclient-set-send-timeout",
                    Fcclient_set_tx_timeout,  2..2,
                    "(cclient timeout-millis)\n\n\
                     Set the send timeout, in milliseconds.")?;

    emacs::register(env, "amplify-elisp/cclient-set-receive-hwm",
                    Fcclient_set_rx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the receive high water mark capacity.")?;

    emacs::register(env, "amplify-elisp/cclient-set-send-hwm",
                    Fcclient_set_tx_hwm,  2..2,
                    "(uclient capacity)\n\n\
                     Set the send high water mark capacity.")?;

    emacs::register(env, "amplify-elisp/cclient-send",
                    Fcclient_send,  2..2,
                    "(cclient msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/cclient-receive",
                    Fcclient_receive,  2..2,
                    "(cclient msg)\n\n\
                     .")?;


    /************************** Msg **************************/
    emacs::register(env, "amplify-elisp/msg-new",
                    Fmsg_new,  0..0,
                    "()\n\n\
                     Create a new message.")?;

    emacs::register(env, "amplify-elisp/msg-set-process",
                    Fmsg_set_process,  2..2,
                    "(msg process)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-process",
                    Fmsg_get_process,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-set-request-number",
                    Fmsg_set_request_number,  2..2,
                    "(msg request-number)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-request-number",
                    Fmsg_get_request_number,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-set-kind",
                    Fmsg_set_kind,  2..2,
                    "(msg kind)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-kind",
                    Fmsg_get_kind,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-set-origin",
                    Fmsg_set_origin,  2..2,
                    "(msg origin)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-origin",
                    Fmsg_get_origin,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-set-contents",
                    Fmsg_set_contents,  2..2,
                    "(msg contents)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-contents",
                    Fmsg_get_contents,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-add-region",
                    Fmsg_add_region,  2..2,
                    "(msg region)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-clear-regions",
                    Fmsg_clear_regions,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-region",
                    Fmsg_get_region,  2..2,
                    "(msg index)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-count-regions",
                    Fmsg_count_regions,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-language",
                    Fmsg_get_language,  1..1,
                    "(msg)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-set-language",
                    Fmsg_set_language,  2..2,
                    "(msg language)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-set-ast",
                    Fmsg_set_ast,  2..2,
                    "(msg ast)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/msg-get-ast",
                    Fmsg_get_ast,  1..1,
                    "(msg)\n\n\
                     .")?;


    /************************** Contents **************************/
    emacs::register(env, "amplify-elisp/contents-new-text",
                    Fcontents_new_text,  1..1,
                    "(text)\n\n\
                     Create a new Contents::Text object.")?;

    emacs::register(env, "amplify-elisp/contents-new-entries",
                    Fcontents_new_entries,  0..1000,
                    "()\n\n\
                     Create a new Contents::Entries object.")?;

    emacs::register(env, "amplify-elisp/contents-is-empty",
                    Fcontents_is_empty,  1..1,
                    "(contents)\n\n\
                     Return t iff. contents is empty, otherwise nil.")?;

    emacs::register(env, "amplify-elisp/contents-is-text",
                    Fcontents_is_text,  1..1,
                    "(contents)\n\n\
                     Return t iff. contents is Contents::Text, otherwise nil.")?;

    emacs::register(env, "amplify-elisp/contents-add-text",
                    Fcontents_add_text,  2..2,
                    "(contents text)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/contents-get-text",
                    Fcontents_get_text,  1..1,
                    "(contents)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/contents-is-entries",
                    Fcontents_is_entries,  1..1,
                    "(contents)\n\n\
                     Return t iff. contents is Contents::Entries, otherwise nil.")?;

    emacs::register(env, "amplify-elisp/contents-add-entry",
                    Fcontents_add_entry,  2..2,
                    "(contents entry)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/contents-get-entry",
                    Fcontents_get_entry,  2..2,
                    "(contents index)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/contents-count-entries",
                    Fcontents_count_entries,  1..1,
                    "(msg)\n\n\
                     .")?;


    /************************** Region **************************/
    emacs::register(env, "amplify-elisp/region-new",
                    Fregion_new,  2..2,
                    "(begin end)\n\n\
                     Create a new Region object.")?;

    emacs::register(env, "amplify-elisp/region-get-begin",
                    Fregion_get_begin,  1..1,
                    "(region)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/region-get-end",
                    Fregion_get_end,  1..1,
                    "(region)\n\n\
                     .")?;


    /************************** Language  **************************/
    emacs::register(env, "amplify-elisp/language-new",
                    Flanguage_new,  1..1,
                    "(name)\n\n\
                     Create a new Language object.")?;

    emacs::register(env, "amplify-elisp/language-set-name",
                    Flanguage_set_name,  2..2,
                    "(language name)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/language-get-name",
                    Flanguage_get_name,  1..1,
                    "(language)\n\n\
                     .")?;


    /************************** Ast **************************/
    emacs::register(env, "amplify-elisp/ast-new",
                    Fast_new,  1..1,
                    "(name)\n\n\
                     Create a new Ast object.")?;

    emacs::register(env, "amplify-elisp/ast-get-name",
                    Fast_get_name,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-set-data",
                    Fast_set_data,  2..2,
                    "(ast data)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-get-data",
                    Fast_get_data,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-clear-data",
                    Fast_clear_data,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-add-child",
                    Fast_add_child,  2..2,
                    "(ast child)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-get-child",
                    Fast_get_child,  2..2,
                    "(ast index)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-clear-children",
                    Fast_clear_children,  1..1,
                    "(ast)\n\n\
                     .")?;

    emacs::register(env, "amplify-elisp/ast-count-children",
                    Fast_count_children,  1..1,
                    "(ast)\n\n\
                     .")?;


    const MODULE_NAME: &str = "amplify-module";
    emacs::provide(env, MODULE_NAME.to_string());
    message!(env, "[amplify-elisp] {} initialized", MODULE_NAME)
}}


emacs_subrs! {
    /************************** UReporter **************************/
    Fureporter_new(env, nargs, args, data, TAG) {
        let reporter = UReporter::new().unwrap(/* TODO: ReportErr */);
        n2e::boxed(env, reporter, emacs::destruct::<UReporter>)
    };

    Fureporter_serialize_using_capnp(env, nargs, args, data, TAG) {
        let ureporter: &mut UReporter = e2n::mut_ref(env, args, 0)?;
        ureporter.set_serialization_method(amplify::Method::CapnProto);
        n2e::symbol(env, "t")
    };

    Fureporter_serialize_using_json(env, nargs, args, data, TAG) {
        let ureporter: &mut UReporter = e2n::mut_ref(env, args, 0)?;
        ureporter.set_serialization_method(amplify::Method::Json);
        n2e::symbol(env, "t")
    };

    Fureporter_set_tx_addr(env, nargs, args, data, TAG) {
        let ureporter: &mut UReporter = e2n::mut_ref(env, args, 0)?;
        let addr = e2n::string(env, *args.offset(1))?;
        let addr = Url::parse(addr.as_str()).unwrap(/* TODO: url ParseError */);
        ureporter.set_send_addr(&addr);
        n2e::symbol(env, "t")
    };

    Fureporter_set_tx_timeout(env, nargs, args, data, TAG) {
        let ureporter: &mut UReporter = e2n::mut_ref(env, args, 0)?;
        let timeout = Timeout::from_number(e2n::integer(env, args, 1)? as isize);
        ureporter.set_send_timeout(timeout);
        n2e::symbol(env, "t")
    };

    Fureporter_set_tx_hwm(env, nargs, args, data, TAG) {
        let ureporter: &mut UReporter = e2n::mut_ref(env, args, 0)?;
        let hwm = Hwm::from_number(e2n::integer(env, args, 1)? as usize);
        ureporter.set_send_hwm(hwm);
        n2e::symbol(env, "t")
    };

    Fureporter_connect(env, nargs, args, data, TAG) {
        let ureporter: &mut UReporter = e2n::mut_ref(env, args, 0)?;
        let creporter: CReporter = ureporter
            .clone(
                /* TODO: The proper solution is being able to take ownership */
                /*       of a pointer back from Elisp. Then this clone won't */
                /*       be necessary anymore.                               */
            )
            .connect().unwrap(/* TODO: ReportErr */);
        n2e::boxed(env, creporter, emacs::destruct::<CReporter>)
    };


    /************************** CReporter **************************/
    Fcreporter_set_tx_timeout(env, nargs, args, data, TAG) {
        let creporter: &mut CReporter = e2n::mut_ref(env, args, 0)?;
        let timeout = Timeout::from_number(e2n::integer(env, args, 1)? as isize);
        creporter.set_send_timeout(timeout).unwrap(/* TODO: ReportErr */);
        n2e::symbol(env, "t")
    };

    Fcreporter_set_tx_hwm(env, nargs, args, data, TAG) {
        let creporter: &mut CReporter = e2n::mut_ref(env, args, 0)?;
        let hwm = Hwm::from_number(e2n::integer(env, args, 1)? as usize);
        creporter.set_send_hwm(hwm).unwrap(/* TODO: ReportErr */);
        n2e::symbol(env, "t")
    };

    Fcreporter_send(env, nargs, args, data, TAG) {
        let creporter: &mut CReporter = e2n::mut_ref(env, args, 0)?;
        let report: &Report = e2n::mut_ref(env, args, 1)?;
        creporter.send(report).unwrap(/* TODO: ReportErr */);
        n2e::symbol(env, "t")
    };


    /************************** Report **************************/
    Freport_new(env, nargs, args, data, TAG) {
        n2e::boxed(env, Report::default(), emacs::destruct::<Report>)
    };

    Freport_set_action(env, nargs, args, data, TAG) {
        let report: &mut Report = e2n::mut_ref(env, args, 0)?;
        *report.action_mut() = e2n::string(env, *args.offset(1))?;
        n2e::symbol(env, "t")
    };

    Freport_get_action(env, nargs, args, data, TAG) {
        let report: &Report = e2n::mut_ref(env, args, 0)?;
        let action: &str = report.action_ref();
        n2e::string(env, action)
    };

    Freport_set_process(env, nargs, args, data, TAG) {
        let report: &mut Report = e2n::mut_ref(env, args, 0)?;
        *report.process_mut() = e2n::string(env, *args.offset(1))?;
        n2e::symbol(env, "t")
    };

    Freport_get_process(env, nargs, args, data, TAG) {
        let report: &Report = e2n::mut_ref(env, args, 0)?;
        let process: &str = report.process_ref();
        n2e::string(env, process)
    };

    Freport_set_request_number(env, nargs, args, data, TAG) {
        let report: &mut Report = e2n::mut_ref(env, args, 0)?;
        let reqno: i64 = e2n::integer(env, args, 1)?;
        if reqno < 0 {
            // TODO: error: reqno >= 0 doesn't hold
        }
        *report.request_number_mut() = reqno as u64;
        n2e::symbol(env, "t")
    };

    Freport_get_request_number(env, nargs, args, data, TAG) {
        let report: &Report = e2n::mut_ref(env, args, 0)?;
        n2e::integer(env, report.request_number() as i64)
    };

    Freport_set_duration_nanos(env, nargs, args, data, TAG) {
        let report: &mut Report = e2n::mut_ref(env, args, 0)?;
        let reqno: i64 = e2n::integer(env, args, 1)?;
        if reqno < 0 {
            // TODO: error: reqno >= 0 doesn't hold
        }
        *report.duration_nanos_mut() = reqno as u64;
        n2e::symbol(env, "t")
    };

    Freport_get_duration_nanos(env, nargs, args, data, TAG) {
        let report: &Report = e2n::mut_ref(env, args, 0)?;
        n2e::integer(env, report.duration_nanos() as i64)
    };

    Freport_set_command(env, nargs, args, data, TAG) {
        let report: &mut Report = e2n::mut_ref(env, args, 0)?;
        let arg: EmacsVal = *args.offset(1);
        *report.command_mut() =
            if emacs::hlapi::is_nil(env, arg)? {  None  }
            else {  Some(e2n::string(env, arg)?)  };
        n2e::symbol(env, "t")
    };

    Freport_get_command(env, nargs, args, data, TAG) {
        let report: &Report = e2n::mut_ref(env, args, 0)?;
        match report.command_ref() {
            Some(cmd) => n2e::string(env, cmd),
            None => n2e::symbol(env, "nil")
        }
    };


    /************************** UClient **************************/
    // Create a GC'd Elisp handle to a CClient value
    Fuclient_new(env, nargs, args, data, TAG) {
        let client = UClient::new().unwrap(/* TODO: ClientErr */);
        n2e::boxed(env, client, emacs::destruct::<UClient>)
    };

    Fuclient_serialize_using_capnp(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        uclient.set_serialization_method(amplify::Method::CapnProto);
        n2e::symbol(env, "t")
    };

    Fuclient_serialize_using_json(env, nargs, args, data, TAG) {
        let uclient: &mut UClient = e2n::mut_ref(env, args, 0)?;
        uclient.set_serialization_method(amplify::Method::Json);
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
        match cclient.send(msg) {
            Ok(()) => n2e::symbol(env, "t"),
            Err(ClientErr::ZmqInterrupted) |
            Err(ClientErr::FailedToSend(ZmqErr::EINTR)) |
            Err(ClientErr::FailedToReceive(ZmqErr::EINTR)) =>
                n2e::symbol(env, "nil"),    // Interrupted, NOP
            Err(ClientErr::ZmqResourceTemporarilyUnavailable) |
            Err(ClientErr::FailedToReceive(ZmqErr::EAGAIN)) |
            Err(ClientErr::FailedToSend(ZmqErr::EAGAIN)) =>
                n2e::symbol(env, "nil"),    // No msg available, NOP
            Err(client_err) => { // Can't handle the error, so reconnect
                println!("[Fcclient_send] Client error: {:?}", client_err);
                message!(env, "[Fcclient_send] Client error: {:?}", client_err)?;
                n2e::symbol(env, ":reconnect")
            },
        }
    };

    Fcclient_receive(env, nargs, args, data, TAG) {
        let cclient: &mut CClient = e2n::mut_ref(env, args, 0)?;
        let msg: &mut Msg = e2n::mut_ref(env, args, 1)?;
        match cclient.receive(msg) {
            Ok(()) => n2e::symbol(env, "t"),
            Err(ClientErr::ZmqInterrupted) |
            Err(ClientErr::FailedToReceive(ZmqErr::EINTR)) =>
                n2e::symbol(env, "nil"),    // Interrupted, NOP
            Err(ClientErr::ZmqResourceTemporarilyUnavailable) |
            Err(ClientErr::FailedToReceive(ZmqErr::EAGAIN)) =>
                n2e::symbol(env, "nil"),   // No msg available, NOP
            Err(client_err) => { // Can't handle the error, so reconnect
                println!("[Fcclient_receive] Client error: {:?}", client_err);
                message!(env, "[Fcclient_receive] Client error: {:?}", client_err)?;
                n2e::symbol(env, ":reconnect")
            },
        }
    };


    /************************** Msg **************************/
    Fmsg_new(env, nargs, args, data, TAG) {
        n2e::boxed(env, Msg::default(), emacs::destruct::<Msg>)
    };

    Fmsg_set_process(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        *msg.process_mut() = e2n::string(env, *args.offset(1))?;
        n2e::symbol(env, "t")
    };

    Fmsg_get_process(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        let process: &str = msg.process_ref();
        n2e::string(env, process)
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

    Fmsg_set_kind(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        *msg.kind_mut() = e2n::string(env, *args.offset(1))?;
        n2e::symbol(env, "t")
    };

    Fmsg_get_kind(env, nargs, args, data, TAG) {
        let msg: &Msg = e2n::mut_ref(env, args, 0)?;
        let kind: &str = msg.kind_ref();
        n2e::string(env, kind)
    };

    Fmsg_set_origin(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        let origin: EmacsVal = *args.offset(1);
        if emacs::hlapi::is_nil(env, origin)? {
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
        if emacs::hlapi::is_nil(env, *args.offset(1))? {
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
        msg.regions_mut().push(*region);
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
        let language: Language = lang_ref
            .map(|lang: &Language| lang.clone(/* TODO: get rid of the clone */))
            .unwrap_or(Language::from(""));
        n2e::boxed(env, language, emacs::destruct::<Language>)
    };

    Fmsg_set_language(env, nargs, args, data, TAG) {
        let msg: &mut Msg = e2n::mut_ref(env, args, 0)?;
        if emacs::hlapi::is_nil(env, *args.offset(1))? {
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
        if emacs::hlapi::is_nil(env, *args.offset(1))? {
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

    Fcontents_is_empty(env, nargs, args, data, TAG) {
        let contents: &Contents = e2n::mut_ref(env, args, 0)?;
        if contents.is_empty() {
            n2e::symbol(env, "t")
        } else {
            n2e::symbol(env, "nil")
        }
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
        n2e::string(env, language.as_ref())
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
        let ast: &Ast = e2n::mut_ref(env, args, 0)?;
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

//  LocalWords:  uclient cclient capn Fmsg Fcclient Ast capnp
//  LocalWords:  ast stringp listp Fcontents aclient AsyncClient
//  LocalWords:  ureporter creporter
