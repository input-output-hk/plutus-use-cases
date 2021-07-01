<template>
  <div>
    <NavBar/>
    <b-row
        v-if="items === undefined || items.length===0">
      <b-col class="mt-5 pt-5 text-center">
        <h3 class="text-muted">Nothing to see here</h3>
      </b-col>
    </b-row>
    <b-container v-else class="container-fluid">
      <b-row v-if="items.length" columns>
        <b-col cols="12" lg="6" v-for="(item, i) in items" :key="i" class="my-2">
          <b-card-group>
            <b-card style="border-radius: 10px">
              <b-card-text>
                <strong>Seller</strong> {{ item.owner }}
              </b-card-text>
              <div v-for="(value,i) in item.values" :key="i">
                <b-card-text>
                  <strong>Currency</strong> {{ value.currency }}
                </b-card-text>
                <b-card-text class="d-flex justify-content-between">
                  <strong>Token</strong> <span>{{ value.token }}</span>
                </b-card-text>
              </div>
              <b-card-text class="d-flex justify-content-between">
                <strong>Cost (Lovelace)</strong> <span class="text-monospace">{{ item.cost.value.toLocaleString() }}</span>
              </b-card-text>
              <b-button
                  :data-label="i"
                  style="align-self: center"
                  variant="primary"
                  @click="onBuy(item)"
              >
                Buy
              </b-button>
            </b-card>
          </b-card-group>
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>
const buffer=require("buffer")
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
  destroyed() {
    if(typeof this.timeoutHandle === "number")
      clearTimeout(this.timeoutHandle)
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
      clearTimeout(this.timeoutHandle)
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/buy`, {
            ppItems: [item.reference],
            ppValue: item.cost
          }).then(() => this.$task.infoMessage("Transaction Submitted."))
      )
      this.timeoutHandle = setTimeout(this.refresh, 10000)
    },
    refresh() {
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/list`, {
            'lmUtxoType': 'MtDirectSale',
          })
      )
      this.timeoutHandle = setTimeout(this.refresh, 5000)
    }
  },
  watch: {
    lastResponse(x) {
      // if (x.length && x.length > 0 && x[0].cost && typeof (x[0].cost.value) == "number" && x[0].owner && x[0].values && x[0].values) {
        this.items = x
      // const encoded = new Buffer(myString).toString('hex'); // encoded === 54686973206973206d7920737472696e6720746f20626520656e636f6465642f6465636f646564
      // const decoded = new Buffer(encoded, 'hex').toString(); // decoded === "This is my string to be encoded/decoded"

      if(this.items !== undefined && this.items.length > 0) {
        this.items.forEach(x=> {
          if (x.values !== undefined && this.items.length > 0)
            x.values.forEach(v => v.token = new buffer.Buffer (v.token,"hex").toString())
        })
      }
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
      timeoutHandle: undefined,
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
