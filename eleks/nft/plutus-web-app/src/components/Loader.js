import { ReactComponent as Spinner } from '../icons/spinner.svg';
import '../styles/Loader.scss';

const Loader = ({ disableBackground, text }) => (
  <div className='Loader'>
    {disableBackground && <div className='cover' />}
    <h5 className={`heading ${disableBackground && 'dark'}`}>
      {text ? text : 'Loading...'}
    </h5>
    <Spinner className='spinner' width='100' />
  </div>
);

export default Loader;
