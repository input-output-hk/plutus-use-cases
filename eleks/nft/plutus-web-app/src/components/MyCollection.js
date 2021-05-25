import React from 'react';
import { partition } from 'ramda';
import { connect } from 'react-redux';
import { compose, lifecycle, withProps } from 'recompose';

import Loader from './Loader';
import TokensList from './TokensList';
import { fetchMyTokens } from '../actions/myTokens';
import { getMyTokens, getMyTokensFetching } from '../reducers';

import '../styles/MyCollection.scss';

const MyCollection = ({ ownedTokens, tokensOnTrade, myTokensFetching }) => (
  <div className='MyCollection'>
    <h3 className='heading'>My collection</h3>

    {!myTokensFetching ? (
      <React.Fragment>
        {tokensOnTrade.length !== 0 && (
          <React.Fragment>
            <h5 className='subheading'>My tokens on trade</h5>
            <TokensList tokens={tokensOnTrade} />
            <div className='line' />
          </React.Fragment>
        )}

        {ownedTokens.length !== 0 && (
          <React.Fragment>
            <h5 className='subheading'>Owned tokens</h5>
            <TokensList tokens={ownedTokens} />
          </React.Fragment>
        )}
      </React.Fragment>
    ) : (
      <Loader />
    )}
  </div>
);

export const enhancer = compose(
  connect(
    (state) => ({
      myTokens: getMyTokens(state),
      myTokensFetching: getMyTokensFetching(state),
    }),
    (dispatch, props) => ({
      fetchMyTokens: () => dispatch(fetchMyTokens(props.currentUser)),
    })
  ),
  withProps(({ myTokens }) => {
    const splitedTokens = partition((token) => token.price, myTokens);
    return {
      ownedTokens: splitedTokens[1],
      tokensOnTrade: splitedTokens[0],
    };
  }),
  lifecycle({
    componentDidMount() {
      this.props.fetchMyTokens();
    },
  })
);

export default enhancer(MyCollection);
