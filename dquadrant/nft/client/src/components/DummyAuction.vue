<template>
  <div>
    <NavBar/>
    <b-row v-if="items === undefined || items.length===0">
      <b-col class="mt-5 pt-5 text-center">
        <h3 class="text-muted">Nothing to see here</h3>
      </b-col>
    </b-row>
    <div v-else class="mt-4 container-fluid">
      <b-row>
        <b-col v-for="(item, i) in items" :key="item.arBidder.bBidReference.txOutRefId.getTxId"
               cols="12" lg="6" class="my-2">
          <div v-for="(content, index) in item.arValue" :key="index">
            <b-card :title="content.token" :sub-title="'Value ' + content.value" class="text-monospace">
              <b-card-text>
                <p><strong>Currency</strong> <span class="text-muted">{{ content.currency }}</span></p>
              </b-card-text>

              <b-card-text>
                <p><strong>Owner</strong> <span class="text-muted">{{ item.arOwner.getPubKeyHash }}</span></p>
                <div v-if="item.arOwner.getPubKeyHash!==item.arBidder.bPubKeyHash.getPubKeyHash">
                  <p>
                    <strong>Bidder</strong> <span class="text-muted">{{
                      item.arBidder.bPubKeyHash.getPubKeyHash
                    }}</span>
                  </p>
                  <p class="d-flex justify-content-between">
                    <strong>Last Bid</strong> <span class="text-muted">{{ item.arBidder.bBid.toLocaleString() }}</span>
                  </p>
                </div>
                <div class="text-muted pb-2" v-else>
                  No Bids yet
                </div>
              </b-card-text>
              <b-button-toolbar aria-label="Bid">
                <b-button-group size="sm" class="mr-1">
                  <b-button variant="primary" @click="onBid(item)">
                    Bid ({{ item.minNewBid.toLocaleString() }})
                  </b-button>
                </b-button-group>
                <b-input-group size="sm" class="mr-1">
                  <b-button variant="success" @click="onBidIncrease(i)">
                    <BIconPatchPlus></BIconPatchPlus>
                  </b-button>
                </b-input-group>
                <b-input-group size="sm">
                  <b-button variant="warning" @click="onBidDecrease(i)">
                    <BIconPatchMinus></BIconPatchMinus>
                  </b-button>
                </b-input-group>
              </b-button-toolbar>
            </b-card>
          </div>
        </b-col>
      </b-row>
    </div>
  </div>
</template>

<script>
import NavBar from "./base/NavBar";

export default {
  name: "DummyAuction",
  components: {NavBar},
  created() {
    this.items = []
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
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/onAuction`, '""')
      )
      this.$store.dispatch('updateAuctionItems', this.items)
    },
    refresh() {
      this.flight = true
      this.$task.do(
          this.$http.post(`instance/${this.instanceId}/endpoint/onAuction`, '""')
      )
      if (this.items.length === 0)
        this.timeoutHandle = setTimeout(this.refresh, 5000)
      else {
        this.$store.dispatch('updateAuctionItems', this.items)
        clearTimeout(this.timeoutHandle)
      }
    },
    onBidIncrease(index) {
      this.$store.dispatch('updateAuctionItems', this.items)
      const item = this.$store.state.auctionItems[index]
      item.minNewBid += item.arMinIncrement
      this.$store.dispatch('updateAuctionItemBidPrice', {item, index})
    },
    onBidDecrease(index) {
      const item = this.$store.state.auctionItems[index]
      if (item.arBidder.bBid + item.arMinIncrement < item.minNewBid) {
        item.minNewBid -= item.arMinIncrement
        this.$store.dispatch('updateAuctionItemBidPrice', {item, index})
      }
      if (item.minNewBid === item.arBidder.bBid + item.arMinIncrement) this.btnDisabled = true
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
