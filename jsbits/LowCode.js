// ------------------------------------ DOM ------------------------------------
class LCElement {
    constructor(value) {
        this.value = value;
    }

    getText() {
        return value.text();
    }

    setText(text) {
        value.text(text);
    }

    onClick(func) {
        value.click(() => func($(this)));
    }

    hide() {
        value.hide();
    }
}

function eventThis() {
    e = e || window.event;
    return e.target || e.srcElement;
}

function getElement(id) {
    return new LCElement($(id));
}
// ---------------------------------- REST API ---------------------------------
function GET(url, before, success, error, complete) {
    let failed = false;
    let res = $.ajax({
        async: false,
        url: url,
        type: "GET",
        beforeSend: (xhr) => {
            xhr.setRequestHeader("Content-type", "application/json");
            before();
        },
        success: success,
        error: (xhr, textStatus, errorThrown) => {
            failed = true;
            return error(xhr.status, textStatus, errorThrown);
        },
        complete: complete,
    });

    if (failed) {
        return { isSuccess: false, result: "" };
    } else {
        return { isSuccess: true, result: res.responseText };
    }
}

function POST(url, value, before, success, error, complete) {
    let failed = false;
    let res = $.ajax({
        async: false,
        url: url,
        type: "POST",
        dataType: "json",
        contentType: "application/json",
        data: value,
        beforeSend: (_) => before(),
        success: success,
        error: (xhr, textStatus, errorThrown) => {
            failed = true;
            return error(xhr.status, textStatus, errorThrown);
        },
        complete: complete,
    });

    if (failed) {
        return { isSuccess: false, result: "" };
    } else {
        return { isSuccess: true, result: res.responseText };
    }
}
// ---------------------------------- Utility ----------------------------------
function trunc(x) {
    return Math.trunc(x);
}

// --------------------------------- Conversion --------------------------------
function doubleToInteger(x) {
    return x | 0;
}

function doubleToText(x) {
    return x.toString();
}

function integerToDouble(x) {
    return x;
}

function integerToText(x) {
    return x.toString();
}

function textToDouble(x) {
    return parseFloat(x);
}

function textToInteger(x) {
    return parseInt(x);
}

function textToIntegerRadix(x, radix) {
    return parseInt(x, radix);
}

// -------------------------------------- UI -----------------------------------
function event(compName, eventType, callback) {
    $(`#${compName}`).on(eventType, callback);
}

function handleHtmlElement(compName, execution) {
    eval(`$("#${compName}").${execution}`);
}
