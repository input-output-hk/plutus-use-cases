<template>
  <div>
    <NavBar></NavBar>
    <div v-if="items.length===0">
      <strong class="text-muted"> Nothing To See Here</strong>
    </div>
    <b-container class="mt-4">
      <b-row v-for="item in items" :key="item.arBidder.bBidReference.txOutRefId.getTxId">
        <b-col>
          <div v-for="(content,index) in item.arValue" :key="index">
            <h4>{{ content.currency + ':' + content.token + ' => ' + content.value }}</h4>
          </div>
          <div class="pb-1">
            <strong>Owner</strong> : {{ item.arOwner.getPubKeyHash }}
          </div>
          <div class="pb-1" v-if="item.arOwner.getPubKeyHash!==item.arBidder.bPubKeyHash.getPubKeyHash">
            <div>
              <strong>Bidder</strong> : {{ item.arBidder.bPubKeyHash.getPubKeyHash }}
            </div>
            <div><strong>Last Bid</strong> : {{ item.arBidder.bBid.toLocaleString() }}</div>
          </div>
          <div class="text-muted pb-2" v-else>
            No Bids yet
          </div>

          <div class="text-right">
            <b-button variant="info" class="" @click="onBid(item)">Bid {{ item.minNewBid.toLocaleString() }}</b-button>
          </div>
          <hr/>
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>
import NavBar from "./base/NavBar";

export default {
  name: "DummyAuction",
  components: {NavBar},
  created() {
    // this.items=[]
    if (this.instanceId) {
      this.refresh()
    }
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
    onBid(item) {
      this.flight = true
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/bid`, {
            "ref": item.arBidder.bBidReference,
            "bidValue": [
              {
                "currency": item.arMinBid.currency,
                "token": item.arMinBid.token,
                "value": item.minNewBid
              }
            ]
          }).then(() => this.$task.infoMessage("Transaction Submitted."))
      )
    },
    refresh() {
      this.flight = true
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/onAuction`, '""')
      )
    }
  },

  watch: {
    lastResponse(newVal) {
      if (this.flight) {
        if (newVal.getTxId) {
          this.$task.successMessage("Transaction Confirmed :" + newVal.getTxId)
        } else {
          const items = JSON.parse(JSON.stringify(newVal))
          items.forEach(x => {
            x.minNewBid = x.arBidder.bBid === 0 ? x.arMinBid.value : (x.arBidder.bBid + x.arMinIncrement)
          })
          this.items = items
        }
        this.flight = false
      }
    },
    instanceId(newVal, oldVal) {
      if (oldVal === undefined) {
        this.refresh()
      }
    }
  },
  data: () => {
    return {
      flight: false,
      items: [
        {
          "arMarketFee": 5000000,
          "arStartTime": [
            {
              "contents": {
                "getPOSIXTime": 0
              },
              "tag": "Finite"
            },
            true
          ],
          "arMinIncrement": 2000000,
          "arValue": [
            {
              "currency": "5e12804044e10921ed3a786b8602ab844eeedfc55dbae30374b287a4bb1e554c",
              "token": "aa",
              "value": 1
            }
          ],
          "arOwner": {
            "getPubKeyHash": "21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9"
          },
          "arBidder": {
            "bBid": 32,
            "bBidReference": {
              "txOutRefIdx": 1,
              "txOutRefId": {
                "getTxId": "874c8b1f8682bf22bd35a13b7d5a5fadb8f7aa775f5dee10bb022c019b32abe6"
              }
            },
            "bPubKeyHash": {
              "getPubKeyHash": "21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9"
            }
          },
          "arEndTime": [
            {
              "contents": {
                "getPOSIXTime": 1624744416
              },
              "tag": "Finite"
            },
            false
          ],
          minNewBid: 32,
          "arMinBid": {
            "currency": "",
            "token": "",
            "value": 10000000
          }
        }
      ]
    }
  }
}
</script>

<style scoped>

</style>
