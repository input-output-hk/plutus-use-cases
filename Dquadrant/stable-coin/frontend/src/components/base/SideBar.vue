<template>
  <div class="px-3 py-2">
    <div class="d-flex flex-column justify-content-lg-end border-bottom align-items-end pb-2">
    <i style="color:gray">My balance</i>
      <b class="balance-style">{{balance}} ada</b>
    </div>
    <b-row v-for="(token,index) in tokens" :key="token.id" class="border-bottom pt-3 pb-3 d-flex justify-content-center align-items-center">
      <b-col>
        {{token.token.substring(0,5)}}......{{token.token.substring(token.token.length-5,)}}
      </b-col>
      <b-col>
        <div v-if="token.isNFT" class="nft-tag-style">
        NFT
        </div>
      </b-col>
      <b-col>
        <b-button v-b-modal="token.id" variant="success" @click="tokenClicked = token.id; selectedIndex = index">
          Sell
        </b-button>
      </b-col>
    </b-row>
    <b-modal :id="tokenClicked" title="Sell Token" ref="my_modal" hide-footer>
      <b-form @submit.prevent="onSell()">
        <b>Token: {{tokens[selectedIndex].token}}</b>
        <b-form-group label="Enter amount" label-for="amount">
          <b-form-input id="amount" v-model="amount" ref="input_number" type="number" placeholder="Enter your amount" required>
          </b-form-input>
        </b-form-group>
        <b-button type="submit" variant="success">Sell</b-button>
      </b-form>
    </b-modal>
  </div>
</template>

<script>
export default{
  name: "SideBar",
  methods:{
    onSell() {
      console.log("Amount response:", this.$refs.input_number.value)
      this.$refs["my_modal"].hide();
    }
  },
  data:() => {
    return{
      balance: 3000,
      amount:"",
      tokenClicked:"",
      selectedIndex:0,
      tokens:[
        {
          id:"1",
          token: "23ir32in2o2okr2koiudqwubqdwoqwidoihhqwd",
          isNFT: true,
        },
        {
          id:"2",
          token: "12der32in2o2okr2koiudqwubqdwoqwidoihhqwd",
          isNFT: false,
        },
        {
          id:"3",
          token: "112332in2o2okr2koiudqwubqdwoqwidoihhqwd",
          isNFT: true,
        },
        {
          id:"4",
          token: "231efw32in2o2okr2koidueqwrubqdwoqwidoihhqwd",
          isNFT: false,
        },
      ],
    }
  }
}
</script>

<style scoped>
.balance-style{
  display: flex;
  justify-content: flex-end;
  font-size: 24px;
}
.nft-tag-style{
  font-size: 12px;
  color: #1EA2C4;
  display: flex;
  justify-content: center;
}
</style>