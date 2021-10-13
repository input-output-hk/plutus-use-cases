import thunk from "redux-thunk";
import { compose } from "ramda";
import { createStore } from "redux";
import rootReducer from "./reducers";
import { applyMiddleware } from "redux";
import { routerMiddleware } from "react-router-redux";

const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;

export default history => {
  return createStore(
    rootReducer(history),
    composeEnhancers(applyMiddleware(routerMiddleware(history), thunk))
  );
};