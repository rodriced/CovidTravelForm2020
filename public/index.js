'use strict';

const lsKey = 'covid.form.data';

var elmApp = Elm.Main.init({
    node: document.getElementById('root'),
    flags: JSON.parse(localStorage.getItem(lsKey))
});

elmApp.ports.storeData.subscribe(function (val) {
    if (val === null) {
        localStorage.removeItem(lsKey);
    } else {
        localStorage.setItem(lsKey, JSON.stringify(val));
    }
});

elmApp.ports.disableRootScrolling.subscribe(function (doIt) {
    var root = document.documentElement;
    if (doIt) {
        root.classList.add('is-clipped');
    } else {
        root.classList.remove('is-clipped');
    }
});

elmApp.ports.print.subscribe(function (id) {
    var toPrint = $(`#${id}`).clone();
    toPrint.find(".dont-print").remove();

    toPrint.printThis();
    // toPrint.printThis({
    //     debug: true
    // });
    // toPrint.printThis({
    //     afterPrint: elmApp.ports.printed.send(null)
    // });
});

