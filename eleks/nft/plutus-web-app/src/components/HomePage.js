import { connect } from 'react-redux';
import { compose, lifecycle } from 'recompose';

import { fetchStorefront } from '../actions/storefront';
import { getStorefront, getStorefrontFetching } from '../reducers';

import Loader from './Loader';
import TokensList from './TokensList';

import '../styles/HomePage.scss';

const HomePage = ({ storefront, storefrontFetching }) => (
  <div className='HomePage'>
    <h3 className='heading'>Storefront</h3>
    {!storefrontFetching && <TokensList tokens={storefront} />}

    {storefrontFetching && <Loader />}
    {storefront.length === 0 && !storefrontFetching && (
      <h5 className='no-text-message'>There is no selling tokens yet</h5>
    )}
  </div>
);

const enhancer = compose(
  connect(
    (state) => ({
      storefront: getStorefront(state),
      storefrontFetching: getStorefrontFetching(state),
    }),
    (dispatch, props) => ({
      fetchStorefront: () =>
        dispatch(fetchStorefront(props.currentUser)),
    })
  ),
  lifecycle({
    componentDidMount() {
      this.props.fetchStorefront();
    },
  })
);

export default enhancer(HomePage);
