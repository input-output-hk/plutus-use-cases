<template>
  <div class="wrapper d-flex align-items-stretch">
    <nav id="sidebar">
      <div class="custom-menu">
        <button type="button" id="sidebarCollapse" @click="sideBarCollapse = !sideBarCollapse" class="btn btn-primary btn-sm" style="cursor: pointer">
          <i class="fa fa-bars" aria-hidden="true"></i><span class="sr-only">My Wallets</span>
        </button>
      </div>
      <div class="p-4">
        <h1><router-link :to="{name: 'market'}" class="logo">Plutus <span>Wallet</span></router-link></h1>
        <ul class="list-unstyled components mb-5">
          <li class="active">
            <div class="d-flex flex-column justify-content-lg-end border-bottom pb-2">
              <i>My balance</i>
              <strong class="balance-style">{{ balance.ada }} Lovelace</strong>
            </div>
          </li>
          <li>
            <a href="#"><span class="fa fa-user"></span> My Wallets</a>
          </li>
          <li>
            <a href="#"><span class="fa fa-briefcase"></span> Mint an NFT</a>
          </li>
          <li>
            <a href="#"><span class="fa fa-sticky-note"></span> Buy / Sell</a>
          </li>
        </ul>
      </div>
    </nav>
    <div id="content">
      <slot></slot>
    </div>
  </div>
</template>

<script>
export default {
  name: "BaseSidebar",
  computed: {
    balance() {
      return this.$store.state.contract.funds
    }
  },
  methods: {
    onSell() {
      this.$task.do(
          this.$http.post(
              `/instance/${this.$store.state.contract.instance.cicContract.unContractInstanceId}/endpoint/sell`
              , [{
                "spItems": [{currency: this.tokenClicked.currency, token: this.tokenClicked.name, value: 1}],
                "spSaleType": "Primary",
                "spCost": {currency: "", token: "", value: parseInt(this.$refs.input_number.value)}
              }]
          ).then(() => {
                this.$task.infoMessage("Transaction Submitted. NFT placed in marketplace")
                this.$bvModal.hide('' + this.tokenClicked)
              }
          )
      )
    },

  },
  data: () => {
    return {
      amount: "",
      tokenClicked: {
        currency: "",
        name: "",
        index: 0
      },
      selectedIndex: 0,
      sideBarCollapse: false,
    }
  }
}
</script>

<style scoped>
.balance-style {
  display: flex;
  justify-content: flex-start;
  font-size: 24px;
}
</style>
