import axios from "axios";
import Vue from "vue";

const apiHost = process.env.VUE_APP_API_URL || "http://localhost:9080/api/contract";
const instance=axios.create({baseURL: apiHost})
instance.interceptors.request.use(
  function(config) {
    const token = localStorage.getItem("user_token");
    if (token) {
      config.headers["Authorization"] = token
    }
    if (config.method !=='get'){
      config.headers['Content-Type']= "application/json"
    }

    return config;
  },
  function(error) {
    return Promise.reject(error);
  }
)
Vue.prototype.$http = instance;
export default instance
