<template>
  <div>
    <NavBar>
    </NavBar>
      <b-container class="cards" style="margin-top: 20px ">
          <b-card-group columns>
            <b-card
                v-for="(item,i) in  items" :key="i"
                style="border-radius: 10px"
            >
              <b-card-text>Name: {{item.name}}</b-card-text>
              <b-card-text>Owner: {{item.owner}}</b-card-text>
              <b-card-text>Cost: {{item.cost}} ADA</b-card-text>
              <b-button
                  :data-label="i"
                  style="align-self: center"
                  @click="onSell(i)"
              >
                Buy
              </b-button>
            </b-card>
          </b-card-group>
      </b-container>2
  </div>

</template>

<script>


import NavBar from "./base/NavBar";
export default {
  name: "Market",
  components: {NavBar},

  // mounted(){
  //   this.$task.do(
  //     this.$http.post(`instance/${this.instanceId}/endpoint/funds`,'').then(x=>console.log(x))
  //   )
  // },
  methods:{

    onSell(index) {
      console.log(this.items[index])
    }
  },
  data:()=>{
    return {
      items:[
        {
          name: "House",
          owner: "Sireto",
          cost: 4.2
        },
        {
          name: "House",
          owner: "Sireto",
          cost: 4.2
        },
        {
          name: "House",
          owner: "Sireto",
          cost: 4.2
        },{
          name: "Car",
          owner: "daimler",
          cost: 4.29
        },{
          name: "Car",
          owner: "daimler",
          cost:94.2
        },{
          name: "nft",
          owner: "PAKKA",
          cost: 34.2
        },{
          name: "nft",
          owner: "PAKKA",
          cost: 41.2
        },
        {
          name: "nft",
          owner: "PAKKA",
          cost: 41.2
        }
      ]
    }
  },

}


</script>

<style scoped>
.enough{
  min-width: 250pt!important;
  border-bottom: 1px !important;
  border-bottom-style: solid!important;
  border-color: #E8E8E8!important;
  border-top: 0;
  border-left: 0;
  border-right: 0;
}
</style>
