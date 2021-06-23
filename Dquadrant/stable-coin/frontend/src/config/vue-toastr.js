import Vue from 'vue'

//import 'vue-toastr/dist/vue-toastr.umd.min'
import VueToastr from "vue-toastr";

Vue.use(VueToastr, {
    defaultTimeout: 5000,
    defaultProgressBar: false,
    defaultProgressBarValue: 0,
    defaultType: "error",
    defaultPosition: "toast-bottom-center",
    defaultCloseOnHover: false,
    defaultClassNames: ["animated", "zoomInUp"]
});
