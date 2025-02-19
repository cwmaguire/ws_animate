"use string";

var {channel, animator} = qsParams();

animation_controls_start(channel,
                         animator,
                         document.body,
                         hasCanvas = true);
window.opener.console.log(`Channel: >${channel}<, animator: >${animator}<`);

function qsParams() {
    // window.location.search: Sets or returns the querystring part of a URL
    const params = new URLSearchParams(window.location.search);
    return {channel: params.get('channel'), animator: params.get('animator')};
}
