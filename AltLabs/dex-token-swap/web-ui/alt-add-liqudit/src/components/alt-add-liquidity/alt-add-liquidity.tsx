import {Component, h, Prop, State} from "@stencil/core";

@Component({
    tag: 'alt-add-liquidity',
    styleUrl: '../../assets/css/style.css',
    shadow: false
})
export class AltSwap {

    @Prop() title: string;

    @State() isDropdownOpen1: boolean = false;
    @State() isDropdownOpen2: boolean = false;
    @State() modalConfirm: boolean = false;
    @State() modalLoading: boolean = false;
    @State() modalSuccess: boolean = false;
    @State() firstAmount: number = null;
    @State() secondAmount: number = null;

    @State() token1: object = {
        symbol: 'ADA',
        img: 'https://ucarecdn.com/c41de605-1e97-4609-9117-fcb3e0aabb78/-/format/jpeg/-/resize/600/',
        name: 'Ada',
        ratio: 1.58
    };
    @State() token2: object = {
        symbol: 'AGI',
        img: 'https://assets.coingecko.com/coins/images/2138/thumb/singularitynet.png?1548609559',
        name: 'Agi',
        ratio: 26.13
    };

    public items : Array<any> = [
        {
            symbol: 'ADA',
            img: 'https://ucarecdn.com/c41de605-1e97-4609-9117-fcb3e0aabb78/-/format/jpeg/-/resize/600/',
            name: 'Ada',
            ratio: 1.58
        },
        {
            symbol: 'AGI',
            img: 'https://assets.coingecko.com/coins/images/2138/thumb/singularitynet.png?1548609559',
            name: 'Agi',
            ratio: 26.13
        },
        {
            symbol: 'REVU',
            img: 'https://assets.coingecko.com/coins/images/7678/small/NZW1UQe6_400x400.png',
            name: 'Revuto',
            ratio: 0.12
        }
    ];

    toggleDropdown1() {
        this.isDropdownOpen2 = false;
        this.isDropdownOpen1 = !this.isDropdownOpen1;
    }
    toggleDropdown2() {
        this.isDropdownOpen1 = false;
        this.isDropdownOpen2 = !this.isDropdownOpen2;
    }
    toggleModalConfirm(bool) {
        this.modalConfirm = bool;
    }
    toggleModalLoading(bool) {
        this.modalLoading = bool;
        this.modalConfirm = false;
        if (bool) {
          const timer = setTimeout(() => {
            this.toggleModalSuccess(true);
            this.firstAmount = null;
            this.secondAmount = null;
          }, 2200);
          return () => clearTimeout(timer);
        }
    }
    toggleModalSuccess(bool) {
        this.modalSuccess = bool;
        this.modalLoading = false;
    }

    setFirstMax() {
        let maxInput = 400;
        this.firstAmount = maxInput;
        this.secondAmount = Math.round((maxInput * this.token1['ratio']/this.token2['ratio'] + Number.EPSILON) * 100) / 100;
    }
    setFirstAmount(e) {
        this.firstAmount = e.target.value;
        this.secondAmount = Math.round((e.target.value * this.token1['ratio']/this.token2['ratio'] + Number.EPSILON) * 100) / 100;
    }

    selectedItem(num, item) {
        if (num === '1') {
            this.token1['symbol'] = item.symbol
            this.token1['img'] = item.img
            this.token1['name'] = item.name
            this.token1['ratio'] = item.ratio
        } else {
            this.token2['symbol'] = item.symbol
            this.token2['img'] = item.img
            this.token2['name'] = item.name
            this.token2['ratio'] = item.ratio
        }
        this.isDropdownOpen1 = false;
        this.isDropdownOpen2 = false;
        this.secondAmount = this.firstAmount * this.token1['ratio']/this.token2['ratio'];
    }

    render() {
        return (
            <div>
                <div class="main-content">
                    <div class="page-content ">
                        <div class="swap"> 
                            <div class="swap__header">
                                <span>{ this.title }</span>
                            </div>
                            <div class="swap__body">
                                <div class="swap__block">
                                    <div class="swap__block-left">
                                        <button type="button" class="select-token selected" onClick={() => this.toggleDropdown1()}>
                                            <img src={this.token1['img']} alt="" />
                                            <div class="details">
                                                <span class="action">Input</span>
                                                <span class="token">{ this.token1['symbol'] }
                                                    <i class="fa fa-angle-down" aria-hidden="true" />
                                                </span>
                                            </div>
                                        </button>
                                        <ul class={ "select-stencil-token " + (this.isDropdownOpen1 ? 'active' : 'inactive') }>
                                            {this.items.map(item =>
                                                <li onClick={() => this.selectedItem('1', item)} class={ (this.token2['symbol'] === item.symbol ? 'not-clickable' : '') }>
                                                    <img src={item.img} alt="" />
                                                    <div>
                                                        <span class="symbol">{item.symbol}</span>
                                                        <span class="name">{item.name}</span>
                                                    </div>
                                                </li>
                                            )}
                                        </ul>
                                    </div>
                                    <div class="swap__block-right">
                                        <div class="swap__block-right-wrapper">
                                            <button type="button" class="btn-max" onClick={() => this.setFirstMax()}>MAX</button>
                                            <input type="number" placeholder="0.0" value={ this.firstAmount } onInput={ (e) => this.setFirstAmount(e)} />
                                        </div>
                                    </div>
                                </div>
                                <div class="swap-plus">
                                    <div class="plus">
                                        <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="#D9DBE9" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"></line><polyline points="19 12 12 19 5 12"></polyline></svg>
                                    </div>
                                </div>
                                <div class="swap__block">
                                    <div class="swap__block-left">
                                        <button type="button" class="select-token selected" onClick={() => this.toggleDropdown2()}>
                                            <img src={this.token2['img']} alt="" />
                                            <div class="details">
                                                <span class="action">Input</span>
                                                <span class="token">{this.token2['symbol']}
                                                    <i class="fa fa-angle-down" aria-hidden="true" />
                                                </span>
                                            </div>
                                        </button>
                                        <ul class={"select-stencil-token " + (this.isDropdownOpen2 ? 'active' : 'inactive')}>
                                            {this.items.map(item =>
                                                <li onClick={() => this.selectedItem('2', item)} class={(this.token1['symbol'] === item.symbol ? 'not-clickable' : '')}>
                                                    <img src={item.img} alt="" />
                                                    <div>
                                                        <span class="symbol">{item.symbol}</span>
                                                        <span class="name">{item.name}</span>
                                                    </div>
                                                </li>
                                            )}
                                        </ul>
                                    </div>
                                    <div class="swap__block-right">
                                        <div class="swap__block-right-wrapper">
                                            <div class="distance">~</div>
                                            <input type="number" placeholder="0.0" value={Math.round((this.secondAmount + Number.EPSILON) * 100) / 100} disabled={true} />
                                        </div>
                                    </div>
                                </div>
                            </div>
                            <div class="swap__footer">
                                <button type="button" class="btn-swap" disabled={ !(this.firstAmount > 0) } onClick={() => this.toggleModalConfirm(true)}>Confirm adding liquidity</button>
                            </div>
                            <div class={'modal-container ' + ( this.modalConfirm ? 'active' : '' )}>
                                <div class="modal-inner">
                                    <button type='button' class="modal-close" onClick={() => this.toggleModalConfirm(false)}>&#10006;</button>
                                    <p>You will receive</p>
                                    <h2 class="mt-auto mb-4 text-center">{ Math.round((this.secondAmount + Number.EPSILON) * 100) / 100} { this.token2['symbol'] }</h2>
                                    <button type="button" class="btn-swap" onClick={() => this.toggleModalLoading(true)}>Confirm adding liquidity</button>
                                </div>
                            </div>
                            <div class={'modal-container loading ' + (this.modalLoading ? 'active' : '')}>
                                <div class="modal-inner">
                                    <button type='button' class="modal-close" onClick={() => this.toggleModalLoading(false)}>&#10006;</button>
                                    <div class="lds-hourglass" />
                                    <h2>Waiting for confirmation</h2>
                                </div>
                            </div>
                            <div class={'modal-container loading ' + (this.modalSuccess ? 'active' : '')}>
                                <div class="modal-inner">
                                    <button type='button' class="modal-close" onClick={() => this.toggleModalSuccess(false)}>&#10006;</button>
                                    <div class="success-sign">&#10003;</div>
                                    <h2>Transaction submited</h2>
                                    <button type="button" class="btn-swap" onClick={() => this.toggleModalSuccess(false)}>Close</button>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
}