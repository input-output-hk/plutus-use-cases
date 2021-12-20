<template>
  <div class="card sketchy-box-shadow">
    <h6 class="card-title">{{cardTitle}}</h6>
    <div class="card-content-item">
      <span class="sig-item-label">
        CurrentPrice ({{priceUnit}})
      </span>
      <h4>{{adaPrice}}</h4>
    </div>
    <div class="card-content-item">
      <span class="sig-item-label">
        Circulating Supply
      </span>
      <h4>{{circulatingSupply}}</h4>
    </div>
  </div>
</template>

<script>

// import {toAda, toAdaStr} from "../../util/stringUtils";

import {toAda, toAdaStr} from "../../util/stringUtils";

export default {
  name: "SigGenCard",
  props:["id", "cardTitle", "currentPrice", "circulatingSupply",
    "currentRatioVal", "isRsv", "reserveRatio"],
  computed: {
    priceUnit(){
      const adaVal = toAda(this.currentPrice)
      if(adaVal >= 1){
        return "ADA"
      }
      return "Lovelace"
    },
    adaPrice(){
      console.log(this.circulatingSupply)
      const adaVal = toAdaStr(this.currentPrice)
      if(adaVal >= 1){
        return adaVal
      }
      return this.currentPrice
    }
  }
}
</script>

<style scoped>
.card{
  padding: 28px;
  background: white;
  border-radius: 12px;
  border: none;
}
.card-content-item{
  margin-bottom: 20px;
  align-items: center;
}
.sig-item-label{
  font-size: 16px;
  font-weight: normal;
  margin-bottom: 12px;
  color: #708090;
}
</style>