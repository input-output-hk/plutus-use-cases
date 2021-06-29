<template>
  <div class="d-flex align-items-stretch">
    <nav id="sidebar" :class="{'active': sideBarCollapse}" style="z-index: 11;">
      <div class="overflow-y">
        <div class="custom-menu" style="cursor: pointer;z-index: 0"
             @click="sideBarCollapse = !sideBarCollapse">
          <span id="sidebarCollapse" class="btn btn-primary p-0 m-0" style="cursor: pointer;z-index: 10;">
            <i class="fa fa-bars mt-4" aria-hidden="true"></i><span class="sr-only">My Wallets</span>
          </span>
        </div>
        <div>
          <h1 class="p-2">
            <router-link :to="{name: 'market'}" class="logo text-decoration-none">
              Plutus <span>Wallet</span>
            </router-link>
          </h1>
          <ul class="list-unstyled components mb-5">
            <li>
              <a href="#" class="p-2 text-decoration-none small text-justify">{{ instanceId }}</a>
            </li>
            <li class="active">
              <div class="d-flex flex-column justify-content-lg-end border-bottom pb-2 p-2">
                My balance
                <strong class="balance-style">
                  {{ (Math.round(balance.ada / 10000) / 100).toLocaleString() }} Ada
                </strong>
              </div>
            </li>
            <li>
              <router-link :to="{name: 'wallets'}" class="btn btn-block text-left py-4 px-2">
                My Wallets
              </router-link>
            </li>
            <li>
              <router-link :to="{name: 'mint'}" class="btn btn-block text-left py-4 px-2">
                Mint an NFT
              </router-link>
            </li>
            <li>
              <router-link :to="{name: 'buysell'}" class="btn btn-block text-left py-4 px-2">
                Buy / Sell
              </router-link>
            </li>
          </ul>
        </div>
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
    },
    instanceId() {
      return this.$store.state.contract.instance.cicContract.unContractInstanceId
    },
  },
  methods: {
  },
  data: () => {
    return {
      sideBarCollapse: false,
      amount: "",
      tokenClicked: {
        currency: "",
        name: "",
        index: 0
      },
      selectedIndex: 0,
      auction: {
        "apMinBid": {
          "currency": "",
          "token": "",
          "value": "2 Ada"
        },
        "apMinIncrement": "2 Ada",
        "apStartTime": Math.floor(Date.now() / 1000),
        "apEndTime": Math.floor(Date.now() / 1000 + 90)
      }
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
