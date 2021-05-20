import { Component, h } from "@stencil/core";


@Component({
    tag: 'c-swap',
    styleUrl: './c-swap.css',
    shadow: false
})
export class SideDrawer {
    //@Prop({reflect: true}) title: string;
    render() {
        return (
            <div>
                <div class="main-content">
                    <div class="page-content">
                        <div class="swap">
                            <div class="swap__header">
                                <span>Swap</span>
                                <button type="button" class="btn-settings"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="3"></circle><path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z"></path></svg></button>
                            </div>
                            <div class="swap__body">
                                <div class="swap__block">
                                    <div class="swap__block-header">
                                        <span>From</span>
                                        <span class="ml-auto">Balance: <span id="balance-token-from">0</span></span>
                                    </div>
                                    <div class="swap__block-body">
                                        <input id="token-amount-from" type="number" placeholder="0.0" autocomplete="off" autocorrect="off" step="0.01" />
                                        <button id="max" type="button" class="btn-max">MAX</button>
                                        <button id="select-token-from" type="button" class="select-token"><span>Select a token</span> <i class="fa fa-angle-down" aria-hidden="true"></i></button>
                                    </div>
                                </div>
                                <div class="swap-rotate"><button type="button" id="rotate" class="btn-rotate"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="#565A69" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"></line><polyline points="19 12 12 19 5 12"></polyline></svg></button></div>
                                <div class="swap__block">
                                    <div class="swap__block-header">
                                        <span>To</span>
                                        <span class="ml-auto"><span id="balance-token-to">-</span></span>
                                    </div>
                                    <div class="swap__block-body">
                                        <input id="token-amount-to" type="number" placeholder="0.0" autocomplete="off" autocorrect="off" step="0.01" readonly />
                                        <button id="select-token-to" type="button" class="select-token"><span>Select a token</span> <i class="fa fa-angle-down" aria-hidden="true"></i></button>
                                    </div>
                                </div>
                            </div>
                            <div class="swap__footer">
                                <button id="swap-button" type="button" disabled class="btn-swap">Enter an amount</button>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="modal fade" id="select-token-modal" tabindex="-1" role="dialog" aria-hidden="true">
                    <div class="modal-dialog" role="document">
                        <div class="modal-content">
                            <div class="modal-header pb-3">
                                <h6 class="modal-title">Select a token</h6>
                                <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                                    <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="sc-kGXeez giNWeI"><line x1="18" y1="6" x2="6" y2="18"></line><line x1="6" y1="6" x2="18" y2="18"></line></svg>
                                </button>
                                <input type="text" id="token-search-input" class="input-search mt-3" placeholder="Search name or paste address" autocomplete="off" value=""></input>
                            </div>
                            <div class="divider mt-1"></div>
                            <div class="modal-body"></div>
                            <div class="modal-footer"></div>
                        </div>
                    </div>
                </div>

            </div>
        );
    }
}