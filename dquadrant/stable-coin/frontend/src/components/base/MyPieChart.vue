<script>
import {Pie} from "vue-chartjs";

export default {
  extends: Pie,
  props: ["data", "options"],
  mounted() {
    this.renderChart(this.data, {
      legend: false,
      borderWidth: "10px",
      hoverBackgroundColor: "red",
      hoverBorderWidth: "10px"
    });
  },
  watch:{
    data(x){
      this.renderChart(x, {
        legend: false,
        borderWidth: "10px",
        hoverBackgroundColor: "red",
        hoverBorderWidth: "10px"
      })
    }
  }
};
</script>

<style scoped>
  canvas{
    width: 300px !important;
    height: 300px !important;
  }
</style>