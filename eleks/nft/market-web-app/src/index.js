import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';
import initStore from "./initStore";
import { Provider } from "react-redux";
import { createBrowserHistory } from 'history';
import { ToastContainer } from "react-toastify";
import { routerMiddleware } from 'react-router-redux';
import { ConnectedRouter } from 'connected-react-router';

import './styles/index.scss';
import "react-toastify/dist/ReactToastify.css";

const history = createBrowserHistory();
export const middleware = routerMiddleware(history);

ReactDOM.render(
  <React.StrictMode>
    <Provider store={initStore(history)}>
      <ConnectedRouter history={history}>
        <App />
        <ToastContainer />
      </ConnectedRouter>
    </Provider>
  </React.StrictMode>,
  document.getElementById('root')
);
