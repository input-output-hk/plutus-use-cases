<template>
  <div class="card">
    <h2 class="card-title">Your balance</h2>
    <div class="h-justified-flex card-content">
      <div v-if="!hasTokens" class="blc-info centered-flex">
        <p>You don't have any assets right now.</p>
      </div>
      <MyPieChart v-if="hasTokens" :data="dataCollection" :options="options"></MyPieChart>
      <div class="blc-details">
        <div class="blc-item h-justified-flex">
          <span class="h-start-flex">
            <div class="blc-item-label-col col-green"/>
            <h6 class="blc-item-label">
              Stable tokens
            </h6>
          </span>
          <h6 class="blc-item-val text-cerulean">
            {{sigUsdVal}}
          </h6>
        </div>
        <div class="blc-item h-justified-flex">
          <span class="h-start-flex">
            <div class="blc-item-label-col col-yellow"/>
            <h6 class="blc-item-label text-parakeet">
              Reserve tokens
            </h6>
          </span>
          <h6 class="blc-item-val">
            {{sigRsvVal}}</h6>
        </div>
        <div class="blc-item h-justified-flex">
          <span class="h-start-flex">
            <div class="blc-item-label-col col-blue"/>
            <h6 class="blc-item-label">
              Ada
            </h6>
          </span>
          <h6 class="blc-item-val">
            {{adaVal}}
          </h6>
        </div>
      </div>
    </div>
    {{this.$store.state.contract.status.observableState[-1]}}
  </div>
</template>

<script>

import MyPieChart from "../base/MyPieChart";
import {toAda} from "../../util/stringUtils";

export default {
  name: "MyBalanceCard",
  components: {MyPieChart},
  props: ["sigUsdVal", "sigRsvVal", "ergVal"],
  data: () => {
    return {
      options: {
        responsive: true,
        legend: {
          display: true,
          position: 'right',
        },
      }
    }
  },
  computed:  {
    adaVal(){
      console.log(this.ergVal)
      return toAda(this.ergVal)
    },
    hasTokens(){
      return this.ergVal > 0 || this.sigUsdVal > 0 || this.sigRsvVal > 0
    },
    dataCollection: {
      get: function(){
        return {
          labels: ['Ada token', 'Stable token', 'Reserve token'],
          datasets: [
            {
              backgroundColor: [ "rgb(92, 92, 211)", '#fa78bf', '#FFF200' ],
              data: [toAda(this.ergVal), this.sigUsdVal, this.sigRsvVal],
            },
          ],
        }
      },
    }
  },
}
</script>

<style scoped>
.card{
  width: 100%;
  /*padding: 28px;*/
  background: white;
  border-radius: 12px;
  border: none;
}
.card-title{
  margin-bottom: 36px;
}
.card-content{
  align-items: center;
  margin-bottom: 24px;
  width: 768px;
  row-gap: 24px;
}
p{
  margin: 0;
  text-align: center;
  white-space: break-spaces;
  word-break: break-word;
}
.blc-info{
  border: 24px solid #f2f2f2;
  border-radius: 50%;
  padding: 8px;
  width: 250px;
  height: 250px;
}
.blc-item{
  margin-bottom: 16px;
  column-gap: 128px;
}
.blc-item-label{
  font-size: 18px;
  font-weight: normal;
}
.blc-item-label-col{
  width:15px;
  height: 15px;
  border-radius: 1px;
  margin-right: 12px;
}
.col-blue{
  background: rgb(92, 92, 211);
}
.col-green{
  background: #fa78bf;
}
.col-yellow{
  background:#FFF200;
}
@media(max-width: 868px){
  .card{
    width: 100% !important;
  }
  .card-content{
    width: 100%;
  }
}
</style>