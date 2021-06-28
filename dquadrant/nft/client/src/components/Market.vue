<template>
  <div>
    <NavBar/>
    <b-container class="cards" style="margin-top: 20px ">
      <b-card-group v-if="items.length" columns>
        <b-card
            v-for="(item,i) in  items" :key="i"
            style="border-radius: 10px"
        >
          <b-card-text>Seller: {{ item.owner }}</b-card-text>
          <div v-for="(value,i) in item.values" :key="i">
            <b-card-text> Currency: {{ value.currency }}</b-card-text>
            <b-card-text> Token: {{ value.token }}</b-card-text>

          </div>
          <b-card-text>Cost: {{ item.cost.value }} Lovelace</b-card-text>
          <b-button
              :data-label="i"
              style="align-self: center"
              @click="onBuy(item)"
          >
            Buy
          </b-button>
        </b-card>
      </b-card-group>
      <b-row v-else>
        <b-col class="mt-5 pt-5 text-center">
          <h3 class="text-muted">Nothing to see here</h3>
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>


import NavBar from "./base/NavBar";

export default {
  name: "Market",
  components: {NavBar},
  created: function () {
    if (this.instanceId) {
      this.refresh()
    }
    this.items = []
  },
  computed: {
    instanceId() {
      return this.$store.state.contract.instance.cicContract.unContractInstanceId
    },
    lastResponse() {
      return this.$store.state.contract.lastObservable
    }
  },
  methods: {
    onBuy(item) {


      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/buy`, {
            ppItems: [item.reference],
            ppValue: item.cost
          }).then(() => this.$task.infoMessage("Transaction Submitted."))
      )
    },
    refresh() {
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/onsale`, '""')
      )
    }
  },
  watch: {
    lastResponse(x) {
      // if (x.length && x.length > 0 && x[0].cost && typeof (x[0].cost.value) == "number" && x[0].owner && x[0].values && x[0].values) {
        this.items = x
      // }
    },
    instanceId(x) {
      if (x) {
        this.refresh()
      }
    }
  },
  data: () => {
    return {
      items: [
        // example value
        {
          "fee": 20000,
          "cost": {
            "currency": "",
            "token": "",
            "value": 200000
          },
          "saleType": "Primary",
          "values": [
            {
              "currency": "5e12804044e10921ed3a786b8602ab844eeedfc55dbae30374b287a4bb1e554c",
              "token": "ac",
              "value": 1
            }
          ],
          "owner": "39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f",
          "reference": {
            "txOutRefIdx": 1,
            "txOutRefId": {
              "getTxId": "fc59674be52d3e043d0f7453fa4a9ee95fdc401156376c3432e764186908f15c"
            }
          }
        }
      ]
    }
  },

}


</script>

<style scoped>
.enough {
  min-width: 250pt !important;
  border-bottom: 1px !important;
  border-bottom-style: solid !important;
  border-color: #E8E8E8 !important;
  border-top: 0;
  border-left: 0;
  border-right: 0;
}
</style>
