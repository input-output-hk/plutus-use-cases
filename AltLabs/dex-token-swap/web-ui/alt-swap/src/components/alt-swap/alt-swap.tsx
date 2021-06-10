import {Component, h, Prop, State} from "@stencil/core";

@Component({
    tag: 'alt-swap',
    styleUrl: '../../assets/css/style.css',
    shadow: false
})
export class AltSwap {

    @Prop() title: string;

    @State() isDropdownOpen1: boolean = false;
    @State() isDropdownOpen2: boolean = false;
    @State() firstAmount: number = null;
    @State() secondAmount: number = null;

    @State() token1: object = {
        symbol: '',
        img: '',
        name: '',
        ratio: null
    };
    @State() token2: object = {
        symbol: '',
        img: '',
        name: '',
        ratio: null
    };

    public items : Array<any> = [
        {
            symbol: 'ADA',
            img: 'https://ucarecdn.com/c41de605-1e97-4609-9117-fcb3e0aabb78/-/format/jpeg/-/resize/600/',
            name: 'Ada',
            ratio: 1
        },
        {
            symbol: 'AGI',
            img: 'https://assets.coingecko.com/coins/images/2138/thumb/singularitynet.png?1548609559',
            name: 'Agi',
            ratio: 1.5
        },
        {
            symbol: 'REVU',
            img: 'https://assets.coingecko.com/coins/images/7678/small/NZW1UQe6_400x400.png',
            name: 'Revuto',
            ratio: 2.8
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
        this.calculateAmount();
    }

    swapPlaces() {
        let pom = {}
        pom = this.token1;
        this.token1 = this.token2;
        this.token2 = pom;
        this.calculateAmount();
    }

    calculateAmount() {
        if (this.token1['symbol'] !== '' && this.token2['symbol'] !== '') {
            if (this.token2['symbol'] === 'ADA') {
                this.secondAmount = 1;
                this.firstAmount =  this.token1['ratio'];
            } else {
                this.firstAmount = 1;
                this.secondAmount =  Math.round((this.token2['ratio'] / this.token1['ratio']  + Number.EPSILON) * 100) / 100;
            }
        }
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
                                    <div class="swap__block-header">
                                        <span>From</span>
                                    </div>
                                    <div class="swap__block-body">
                                        <input type="number" value={this.firstAmount} placeholder="0.0" autocomplete="off" autocorrect="off" step="0.01" disabled />
                                        <button type="button" class={ "select-token " + (this.token1['symbol'] !== '' ? 'selected' : '')}  onClick={() => this.toggleDropdown1()}>
                                            { this.token1['symbol'] !== '' ?
                                                <span>
                                                    <img src={this.token1['img']} alt="" />
                                                    { this.token1['symbol'] }
                                                </span>
                                                :
                                                <span>Select a token</span>
                                            }
                                            <i class="fa fa-angle-down" aria-hidden="true" />
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
                                </div>
                                <div class="swap-rotate" onClick={() => this.swapPlaces()}>
                                    <button type="button" class="btn-rotate"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="#D9DBE9" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"></line><polyline points="19 12 12 19 5 12"></polyline></svg></button>
                                </div>
                                <div class="swap__block">
                                    <div class="swap__block-header">
                                        <span>To</span>
                                    </div>
                                    <div class="swap__block-body">
                                        <input type="number" value={this.secondAmount} placeholder="0.0" autocomplete="off" autocorrect="off" step="0.01" disabled />
                                        <button type="button" class={ "select-token " + (this.token2['symbol'] !== '' ? 'selected' : '')}  onClick={() => this.toggleDropdown2()}>
                                            {this.token2['symbol'] !== '' ?
                                                <span>
                                                    <img src={this.token2['img']} alt="" />
                                                    { this.token2['symbol'] }
                                                </span>
                                                :
                                                <span>Select a token</span>
                                            }
                                            <i class="fa fa-angle-down" aria-hidden="true" />
                                        </button>
                                        <ul class={ "select-stencil-token " + (this.isDropdownOpen2 ? 'active' : 'inactive') }>
                                            {this.items.map(item =>
                                                <li onClick={() => this.selectedItem('2', item)} class={ (this.token1['symbol'] === item.symbol ? 'not-clickable' : '') }>
                                                    <img src={item.img} alt="" />
                                                    <div>
                                                        <span class="symbol">{item.symbol}</span>
                                                        <span class="name">{item.name}</span>
                                                    </div>
                                                </li>
                                            )}
                                        </ul>
                                    </div>
                                </div>
                            </div>
                            <div class="swap__footer">
                                <button type="button" class="btn-swap" disabled={ this.token1['symbol'] === '' || this.token2['symbol'] === '' }>SWAP</button>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        );
    }
}