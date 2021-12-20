<template>
  <button class="btn"
          v-b-modal="modalId"
          :disabled = "disabled"
          @click.stop.prevent="onClick">
    <span class="btn-text">
      {{btnText}}
    </span>
  </button>
</template>

<script>
export default {
  name: "MyButton",
  props: ["btnText", "modalId", "showLoading", "disabled"],

  data(){
    return {
      isLoading: false,
    }
  },

  methods:{
    async onClick() {
      this.isLoading = true;
      console.log("in button")
      await this.$emit("click-btn")
      this.isLoading = false;
    }
  }
}
</script>

<style scoped>
.btn{
  padding: 12px 20px;
  min-height: 40px;
  min-width: 70px;
  background: #5c5cd3;
}
.btn.disabled, .btn:disabled{
  background: darkgrey;
  cursor: not-allowed;
}
.btn-text{
  font-size: 16px;
  font-weight: bold;
  color: white;
}
</style>