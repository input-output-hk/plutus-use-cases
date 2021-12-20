<template>
  <div class="mx-3 my-2">
    <div class="d-flex flex-column justify-content-lg-end border-bottom align-items-end pb-2">
      <i style="color:gray">My balance</i>
      <b class="balance-style">{{ balance.ada }} Lovelace</b>
    </div>
    <b-container>
      <b-row v-for="(token,index) in balance.tokens" :key="index"
             class="border-bottom mt-3 mb-3 d-flex justify-content-center align-items-center">
        <b-col class="col-8 py-1" >
          {{ token.currency.substring(0, 3) }}<span
            class="text-muted">...</span>{{ token.currency.substring(token.currency.length - 3) }}<b class="text-lg-left
"> : </b>{{ token.name }}
        </b-col>
        <b-col class="col-2 p-1">
          <div v-if="token.value===1" class="nft-tag-style">
            NFT
          </div>
        </b-col>
        <b-col class="col-2 p-1">
          <b-button v-b-modal="''+token.index" variant="success" @click="tokenClicked = token;">
            Sell
          </b-button>
        </b-col>
      </b-row>
    </b-container>
    <b-modal size="lg" :id="''+tokenClicked.index" title="Sell Token" ref="my_modal" hide-footer>
      <b-form @submit.prevent="onSell()">
        <div class="py-1"><strong>Policy : </strong><span class="text-sm-left">{{ tokenClicked.currency }}</span></div>
        <div class="py-1"><strong>Token:</strong>&nbsp; {{ tokenClicked.name }}</div>
        <b-form-input class="my-3" id="amount" v-model="amount" ref="input_number" type="number" placeholder="Enter Sell price in Lovelace"
                      required>
        </b-form-input>
        <b-button class="my-2" type="submit" variant="success">Sell</b-button>
      </b-form>
    </b-modal>
  </div>
</template>

<script>
export default {
  name: "SideBar",
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
                "spItems":[{currency: this.tokenClicked.currency,token:this.tokenClicked.name,value:1}],
                "spSaleType": "Primary",
                "spCost":{currency: "",token:"",value:parseInt(this.$refs.input_number.value)}
              }]
          ).then(() => {
                this.$task.infoMessage("Transaction Submitted. NFT placed in marketplace")
                this.$bvModal.hide(''+this.tokenClicked)
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
    }
  }
}
</script>

<style scoped>
.balance-style {
  display: flex;
  justify-content: flex-end;
  font-size: 24px;
}

.nft-tag-style {
  font-size: 12px;
  color: #1EA2C4;
  display: flex;
  justify-content: center;
}
</style>
